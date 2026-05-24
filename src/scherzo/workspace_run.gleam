import gleam/dict.{type Dict}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/orchestrator/schedule_core
import scherzo/path
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_dag
import scherzo/workflow_identity
import scherzo/workspace
import scherzo/workspace_driver_lifecycle
import simplifile

pub type PreparedStepWorkspace {
  PreparedStepWorkspace(
    workflow_id: String,
    run_id: String,
    run_root: String,
    workflow_bundle_dir: String,
    attempt_index: Int,
    workspace_name: String,
    path: String,
    source_workspace_name: Option(String),
    source_workspace_path: Option(String),
    workspace_profile: String,
  )
}

pub type PrepareError {
  WorkspaceFailure(error.WorkspaceError)
  HookFailure(error.HookError)
}

pub fn prepare_step(
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  step_id: String,
  workspace_ref: workflow_dag.WorkspaceRef,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
  known_workspaces: Dict(String, PreparedStepWorkspace),
) -> Result(PreparedStepWorkspace, PrepareError) {
  prepare_step_attempt(
    issue,
    workflow_id,
    run_id,
    step_id,
    1,
    workspace_ref,
    orchestrator,
    profile,
    known_workspaces,
  )
}

pub fn prepare_step_attempt(
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  step_id: String,
  attempt_index: Int,
  workspace_ref: workflow_dag.WorkspaceRef,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
  known_workspaces: Dict(String, PreparedStepWorkspace),
) -> Result(PreparedStepWorkspace, PrepareError) {
  prepare_step_attempt_with_cleanup(
    issue,
    workflow_id,
    run_id,
    step_id,
    attempt_index,
    workspace_ref,
    orchestrator,
    profile,
    known_workspaces,
    True,
  )
}

pub fn prepare_recovered_step(
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  expected_run_root: String,
  step_id: String,
  workspace_ref: workflow_dag.WorkspaceRef,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
  known_workspaces: Dict(String, PreparedStepWorkspace),
) -> Result(PreparedStepWorkspace, PrepareError) {
  prepare_recovered_step_attempt(
    issue,
    workflow_id,
    run_id,
    expected_run_root,
    step_id,
    1,
    workspace_ref,
    orchestrator,
    profile,
    known_workspaces,
  )
}

pub fn prepare_recovered_step_attempt(
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  expected_run_root: String,
  step_id: String,
  attempt_index: Int,
  workspace_ref: workflow_dag.WorkspaceRef,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
  known_workspaces: Dict(String, PreparedStepWorkspace),
) -> Result(PreparedStepWorkspace, PrepareError) {
  use _ <- try_prepare(validate_expected_run_root(
    issue,
    workflow_id,
    run_id,
    expected_run_root,
    orchestrator,
  ))
  let known_workspaces = known_workspaces
  case reusable_workspace(workspace_ref, known_workspaces) {
    Some(prepared) -> {
      use _ <- try_prepare(validate_recovered_workspace(
        prepared,
        workflow_id,
        run_id,
        expected_run_root,
        workspace_ref.name,
        profile.name,
        orchestrator,
      ))
      reuse_prepared_workspace(
        issue,
        step_id,
        prepared,
        attempt_index,
        orchestrator,
        profile,
      )
    }
    None -> {
      use _ <- try_prepare(validate_recovered_source_workspace(
        workspace_ref.from,
        known_workspaces,
        workflow_id,
        run_id,
        expected_run_root,
        profile.name,
        orchestrator,
      ))
      prepare_step_attempt_with_cleanup(
        issue,
        workflow_id,
        run_id,
        step_id,
        attempt_index,
        workspace_ref,
        orchestrator,
        profile,
        known_workspaces,
        False,
      )
    }
  }
}

fn prepare_step_attempt_with_cleanup(
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  step_id: String,
  attempt_index: Int,
  workspace_ref: workflow_dag.WorkspaceRef,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
  known_workspaces: Dict(String, PreparedStepWorkspace),
  cleanup_on_error: Bool,
) -> Result(PreparedStepWorkspace, PrepareError) {
  case reusable_workspace(workspace_ref, known_workspaces) {
    Some(prepared) ->
      reuse_prepared_workspace(
        issue,
        step_id,
        prepared,
        attempt_index,
        orchestrator,
        profile,
      )
    None -> {
      use paths <- try_prepare(workspace_paths(
        issue,
        workflow_id,
        run_id,
        step_id,
        attempt_index,
        workspace_ref.name,
        orchestrator,
      ))
      let #(run_root, workspace_path) = paths
      use source <- try_prepare(source_workspace(
        workspace_ref.from,
        known_workspaces,
        profile.name,
      ))
      let #(source_name, source_path) = source
      use _ <- try_prepare(validate_source_directory(source_path))
      use _ <- try_prepare(create_directory(run_root))
      let prepared =
        PreparedStepWorkspace(
          workflow_id: workflow_id,
          run_id: run_id,
          run_root: run_root,
          workflow_bundle_dir: workflow_identity.workflow_bundle_dir(
            orchestrator,
            workflow_id,
          ),
          attempt_index: attempt_index,
          workspace_name: workspace_ref.name,
          path: workspace_path,
          source_workspace_name: source_name,
          source_workspace_path: source_path,
          workspace_profile: profile.name,
        )
      case
        finish_prepare_step(issue, step_id, prepared, orchestrator, profile)
      {
        Ok(prepared) -> Ok(prepared)
        Error(err) -> {
          case cleanup_on_error {
            True -> {
              let _ = cleanup_run(run_root, orchestrator, profile)
              Nil
            }
            False -> Nil
          }
          Error(err)
        }
      }
    }
  }
}

fn finish_prepare_step(
  issue: tracker_issue.Issue,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Result(PreparedStepWorkspace, PrepareError) {
  use _ <- result.try(run_create_hook(
    issue,
    step_id,
    prepared,
    orchestrator,
    profile,
  ))
  use _ <- try_prepare(ensure_directory_after_create(prepared.path))
  use _ <- result.try(run_before_step_hook(
    issue,
    step_id,
    prepared,
    orchestrator,
    profile,
  ))
  Ok(prepared)
}

fn finish_prepare_scheduled_step(
  scheduled: schedule_core.ScheduledRunContext,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Result(PreparedStepWorkspace, PrepareError) {
  use _ <- result.try(run_scheduled_create_hook(
    scheduled,
    step_id,
    prepared,
    orchestrator,
    profile,
  ))
  use _ <- try_prepare(ensure_directory_after_create(prepared.path))
  use _ <- result.try(run_scheduled_before_step_hook(
    scheduled,
    step_id,
    prepared,
    orchestrator,
    profile,
  ))
  Ok(prepared)
}

pub fn after_step(
  issue: tracker_issue.Issue,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Nil {
  case profile.driver {
    Some(driver) ->
      workspace_driver_lifecycle.run_best_effort(
        "driver_lifecycle_after_step",
        config_types.LifecycleAfterStep,
        driver,
        orchestrator,
        hook_env(issue, step_id, prepared, orchestrator),
      )
    None -> Nil
  }
}

pub fn scheduled_after_step(
  scheduled: schedule_core.ScheduledRunContext,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Nil {
  let env =
    scheduled_hook_env(
      scheduled.job_id,
      schedule_core.iso_utc(scheduled.due_at_ms),
      schedule_core.iso_utc(scheduled.started_at_ms),
      scheduled.attempt,
      step_id,
      prepared,
      orchestrator,
    )
  case profile.driver {
    Some(driver) ->
      workspace_driver_lifecycle.run_best_effort(
        "driver_lifecycle_after_step",
        config_types.LifecycleAfterStep,
        driver,
        orchestrator,
        env,
      )
    None -> Nil
  }
}

pub fn cleanup_run(
  run_root: String,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, error.WorkspaceError) {
  let root_abs =
    path.absolute(orchestrator.effective.workspace.root)
    |> result.unwrap(orchestrator.effective.workspace.root)
  let target_abs = path.absolute(run_root) |> result.unwrap(run_root)
  case
    string.trim(target_abs) == ""
    || !path.contains(root_abs, target_abs)
    || target_abs == root_abs
  {
    True -> Error(error.WorkspaceOutsideRoot(target_abs))
    False -> {
      case retain_cleanup(target_abs) {
        True -> Ok(Nil)
        False -> {
          use _ <- result.try(run_remove_lifecycle(
            target_abs,
            orchestrator,
            profile,
          ))
          case simplifile.delete(target_abs) {
            Ok(Nil) -> Ok(Nil)
            Error(simplifile.Enoent) -> Ok(Nil)
            Error(_) -> Error(error.WorkspaceIo("delete failed"))
          }
        }
      }
    }
  }
}

pub fn cleanup_retention_marker(run_root: String) -> String {
  path.join(run_root, ".scherzo-keep-workspace")
}

pub fn workspace_path_for(
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  workspace_name: String,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(String, error.WorkspaceError) {
  workspace_path_for_attempt(
    issue,
    workflow_id,
    run_id,
    "step",
    1,
    workspace_name,
    orchestrator,
  )
}

pub fn workspace_path_for_attempt(
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  step_id: String,
  attempt_index: Int,
  workspace_name: String,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(String, error.WorkspaceError) {
  use paths <- try_workspace(workspace_paths(
    issue,
    workflow_id,
    run_id,
    step_id,
    attempt_index,
    workspace_name,
    orchestrator,
  ))
  let #(_, workspace_path) = paths
  Ok(workspace_path)
}

pub fn prepare_scheduled_step_attempt(
  scheduled: schedule_core.ScheduledRunContext,
  step_id: String,
  workspace_ref: workflow_dag.WorkspaceRef,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
  known_workspaces: Dict(String, PreparedStepWorkspace),
) -> Result(PreparedStepWorkspace, PrepareError) {
  case reusable_workspace(workspace_ref, known_workspaces) {
    Some(prepared) ->
      reuse_scheduled_prepared_workspace(
        scheduled,
        step_id,
        prepared,
        orchestrator,
        profile,
      )
    None -> {
      use paths <- try_prepare(scheduled_workspace_paths(
        scheduled.job_id,
        scheduled.workflow_id,
        scheduled.run_id,
        workspace_ref.name,
        orchestrator,
      ))
      let #(run_root, workspace_path) = paths
      use source <- try_prepare(source_workspace(
        workspace_ref.from,
        known_workspaces,
        profile.name,
      ))
      let #(source_name, source_path) = source
      use _ <- try_prepare(validate_source_directory(source_path))
      use _ <- try_prepare(create_directory(run_root))
      let prepared =
        PreparedStepWorkspace(
          workflow_id: scheduled.workflow_id,
          run_id: scheduled.run_id,
          run_root: run_root,
          workflow_bundle_dir: workflow_identity.workflow_bundle_dir(
            orchestrator,
            scheduled.workflow_id,
          ),
          attempt_index: scheduled.attempt,
          workspace_name: workspace_ref.name,
          path: workspace_path,
          source_workspace_name: source_name,
          source_workspace_path: source_path,
          workspace_profile: profile.name,
        )
      case
        finish_prepare_scheduled_step(
          scheduled,
          step_id,
          prepared,
          orchestrator,
          profile,
        )
      {
        Ok(prepared) -> Ok(prepared)
        Error(err) -> {
          let _ = cleanup_run(run_root, orchestrator, profile)
          Error(err)
        }
      }
    }
  }
}

pub fn scheduled_workspace_path_for_attempt(
  job_id: String,
  workflow_id: String,
  run_id: String,
  _step_id: String,
  _attempt_index: Int,
  workspace_name: String,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(String, error.WorkspaceError) {
  use paths <- try_workspace(scheduled_workspace_paths(
    job_id,
    workflow_id,
    run_id,
    workspace_name,
    orchestrator,
  ))
  let #(_, workspace_path) = paths
  Ok(workspace_path)
}

pub fn scheduled_run_root_for(
  job_id: String,
  workflow_id: String,
  run_id: String,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(String, error.WorkspaceError) {
  use paths <- try_workspace(scheduled_workspace_paths(
    job_id,
    workflow_id,
    run_id,
    "main",
    orchestrator,
  ))
  let #(run_root, _) = paths
  Ok(run_root)
}

pub fn run_root_for(
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(String, error.WorkspaceError) {
  use issue_key <- try_workspace(workspace.sanitize(issue.identifier))
  use workflow_key <- try_workspace(workspace.sanitize(workflow_id))
  use run_key <- try_workspace(workspace.sanitize(run_id))
  let root_abs =
    path.absolute(orchestrator.effective.workspace.root)
    |> result.unwrap(orchestrator.effective.workspace.root)
  let issue_root = path.join(path.join(root_abs, workflow_key), issue_key)
  let run_root = path.join(issue_root, run_key)
  let run_root_abs = path.absolute(run_root) |> result.unwrap(run_root)
  case path.contains(root_abs, run_root_abs) {
    True -> Ok(run_root_abs)
    False -> Error(error.WorkspaceOutsideRoot(run_root_abs))
  }
}

fn validate_expected_run_root(
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  expected_run_root: String,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(Nil, error.WorkspaceError) {
  use computed <- try_workspace(run_root_for(
    issue,
    workflow_id,
    run_id,
    orchestrator,
  ))
  let expected_abs =
    path.absolute(expected_run_root) |> result.unwrap(expected_run_root)
  case computed == expected_abs {
    True -> Ok(Nil)
    False -> Error(error.WorkspaceIo("recovered run root mismatch"))
  }
}

fn validate_recovered_workspace(
  prepared: PreparedStepWorkspace,
  workflow_id: String,
  run_id: String,
  expected_run_root: String,
  workspace_name: String,
  profile_name: String,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(Nil, error.WorkspaceError) {
  let expected_abs =
    path.absolute(expected_run_root) |> result.unwrap(expected_run_root)
  let prepared_run_root_abs =
    path.absolute(prepared.run_root) |> result.unwrap(prepared.run_root)
  let root_abs =
    path.absolute(orchestrator.effective.workspace.root)
    |> result.unwrap(orchestrator.effective.workspace.root)
  let prepared_path_abs =
    path.absolute(prepared.path) |> result.unwrap(prepared.path)
  case
    prepared.workflow_id == workflow_id
    && prepared.run_id == run_id
    && prepared.workspace_name == workspace_name
    && prepared.workspace_profile == profile_name
    && prepared_run_root_abs == expected_abs
    && prepared_path_abs != expected_abs
    && path.contains(expected_abs, prepared_path_abs)
    && path.contains(root_abs, prepared_path_abs)
    && path.contains(root_abs, prepared_run_root_abs)
  {
    True -> validate_existing_directory(prepared.path)
    False -> Error(error.WorkspaceIo("invalid recovered workspace"))
  }
}

fn validate_recovered_source_workspace(
  source_name: Option(String),
  known_workspaces: Dict(String, PreparedStepWorkspace),
  workflow_id: String,
  run_id: String,
  expected_run_root: String,
  profile_name: String,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(Nil, error.WorkspaceError) {
  case source_name {
    None -> Ok(Nil)
    Some(name) ->
      case dict.get(known_workspaces, name) {
        Error(_) -> Ok(Nil)
        Ok(prepared) ->
          validate_recovered_workspace(
            prepared,
            workflow_id,
            run_id,
            expected_run_root,
            name,
            profile_name,
            orchestrator,
          )
      }
  }
}

fn validate_source_directory(
  source_path: Option(String),
) -> Result(Nil, error.WorkspaceError) {
  case source_path {
    None -> Ok(Nil)
    Some(path) -> validate_existing_directory(path)
  }
}

fn validate_existing_directory(
  path: String,
) -> Result(Nil, error.WorkspaceError) {
  case simplifile.is_directory(path) {
    Ok(True) -> Ok(Nil)
    Ok(False) -> Error(error.WorkspaceIo("source workspace missing"))
    Error(_) -> Error(error.WorkspaceIo("source workspace missing"))
  }
}

fn workspace_paths(
  issue: tracker_issue.Issue,
  workflow_id: String,
  run_id: String,
  _step_id: String,
  _attempt_index: Int,
  workspace_name: String,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(#(String, String), error.WorkspaceError) {
  use issue_key <- try_workspace(workspace.sanitize(issue.identifier))
  use workflow_key <- try_workspace(workspace.sanitize(workflow_id))
  use run_key <- try_workspace(workspace.sanitize(run_id))
  use workspace_key <- try_workspace(workspace.sanitize(workspace_name))
  let root_abs =
    path.absolute(orchestrator.effective.workspace.root)
    |> result.unwrap(orchestrator.effective.workspace.root)
  let issue_root = path.join(path.join(root_abs, workflow_key), issue_key)
  let run_root = path.join(issue_root, run_key)
  let workspace_path =
    path.join(path.join(run_root, "workspaces"), workspace_key)
  let run_root_abs = path.absolute(run_root) |> result.unwrap(run_root)
  let workspace_abs =
    path.absolute(workspace_path) |> result.unwrap(workspace_path)
  case
    path.contains(root_abs, run_root_abs)
    && path.contains(root_abs, workspace_abs)
  {
    True -> Ok(#(run_root_abs, workspace_abs))
    False -> Error(error.WorkspaceOutsideRoot(workspace_abs))
  }
}

fn scheduled_workspace_paths(
  job_id: String,
  workflow_id: String,
  run_id: String,
  workspace_name: String,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(#(String, String), error.WorkspaceError) {
  use job_key <- try_workspace(workspace.sanitize(job_id))
  use workflow_key <- try_workspace(workspace.sanitize(workflow_id))
  use run_key <- try_workspace(workspace.sanitize(run_id))
  use workspace_key <- try_workspace(workspace.sanitize(workspace_name))
  let root_abs =
    path.absolute(orchestrator.effective.workspace.root)
    |> result.unwrap(orchestrator.effective.workspace.root)
  let scheduled_root = path.join(path.join(root_abs, workflow_key), "scheduled")
  let job_root = path.join(scheduled_root, job_key)
  let run_root = path.join(job_root, run_key)
  let workspace_path =
    path.join(path.join(run_root, "workspaces"), workspace_key)
  let run_root_abs = path.absolute(run_root) |> result.unwrap(run_root)
  let workspace_abs =
    path.absolute(workspace_path) |> result.unwrap(workspace_path)
  case
    path.contains(root_abs, run_root_abs)
    && path.contains(root_abs, workspace_abs)
  {
    True -> Ok(#(run_root_abs, workspace_abs))
    False -> Error(error.WorkspaceOutsideRoot(workspace_abs))
  }
}

fn retain_cleanup(run_root: String) -> Bool {
  case simplifile.is_file(cleanup_retention_marker(run_root)) {
    Ok(True) -> True
    _ -> False
  }
}

fn reusable_workspace(
  workspace_ref: workflow_dag.WorkspaceRef,
  known_workspaces: Dict(String, PreparedStepWorkspace),
) -> Option(PreparedStepWorkspace) {
  let should_reuse = case workspace_ref.from {
    None -> True
    Some(source) -> source == workspace_ref.name
  }
  case should_reuse {
    False -> None
    True ->
      case dict.get(known_workspaces, workspace_ref.name) {
        Ok(prepared) -> Some(prepared)
        Error(_) -> None
      }
  }
}

fn reuse_prepared_workspace(
  issue: tracker_issue.Issue,
  step_id: String,
  prepared: PreparedStepWorkspace,
  attempt_index: Int,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Result(PreparedStepWorkspace, PrepareError) {
  use _ <- try_prepare(validate_prepared_workspace_profile(
    prepared,
    profile.name,
  ))
  let prepared =
    PreparedStepWorkspace(
      ..prepared,
      attempt_index: attempt_index,
      source_workspace_name: Some(prepared.workspace_name),
      source_workspace_path: Some(prepared.path),
    )
  use _ <- try_prepare(ensure_directory_after_create(prepared.path))
  use _ <- result.try(run_before_step_hook(
    issue,
    step_id,
    prepared,
    orchestrator,
    profile,
  ))
  Ok(prepared)
}

fn reuse_scheduled_prepared_workspace(
  scheduled: schedule_core.ScheduledRunContext,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Result(PreparedStepWorkspace, PrepareError) {
  use _ <- try_prepare(validate_prepared_workspace_profile(
    prepared,
    profile.name,
  ))
  let prepared =
    PreparedStepWorkspace(
      ..prepared,
      attempt_index: scheduled.attempt,
      source_workspace_name: Some(prepared.workspace_name),
      source_workspace_path: Some(prepared.path),
    )
  use _ <- try_prepare(ensure_directory_after_create(prepared.path))
  use _ <- result.try(run_scheduled_before_step_hook(
    scheduled,
    step_id,
    prepared,
    orchestrator,
    profile,
  ))
  Ok(prepared)
}

fn source_workspace(
  from: Option(String),
  known_workspaces: Dict(String, PreparedStepWorkspace),
  profile_name: String,
) -> Result(#(Option(String), Option(String)), error.WorkspaceError) {
  case from {
    None -> Ok(#(None, None))
    Some(name) ->
      case dict.get(known_workspaces, name) {
        Ok(prepared) -> {
          use _ <- try_workspace(validate_prepared_workspace_profile(
            prepared,
            profile_name,
          ))
          Ok(#(Some(name), Some(prepared.path)))
        }
        Error(_) ->
          Error(error.WorkspaceIo("source workspace is not prepared: " <> name))
      }
  }
}

fn validate_prepared_workspace_profile(
  prepared: PreparedStepWorkspace,
  profile_name: String,
) -> Result(Nil, error.WorkspaceError) {
  case prepared.workspace_profile == profile_name {
    True -> Ok(Nil)
    False -> Error(error.WorkspaceIo("prepared workspace profile mismatch"))
  }
}

fn run_create_hook(
  issue: tracker_issue.Issue,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, PrepareError) {
  let env = hook_env(issue, step_id, prepared, orchestrator)
  case profile.driver {
    Some(driver) ->
      run_driver_lifecycle_or_create_directory(
        "driver_lifecycle_create",
        config_types.LifecycleCreate,
        driver,
        prepared,
        orchestrator,
        env,
      )
    None ->
      create_directory(prepared.path) |> result.map_error(WorkspaceFailure)
  }
}

fn run_before_step_hook(
  issue: tracker_issue.Issue,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, PrepareError) {
  let env = hook_env(issue, step_id, prepared, orchestrator)
  case profile.driver {
    Some(driver) ->
      run_driver_lifecycle_if_supported(
        "driver_lifecycle_before_step",
        config_types.LifecycleBeforeStep,
        driver,
        prepared,
        orchestrator,
        env,
      )
    None -> Ok(Nil)
  }
}

fn run_scheduled_create_hook(
  scheduled: schedule_core.ScheduledRunContext,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, PrepareError) {
  let env =
    scheduled_hook_env(
      scheduled.job_id,
      schedule_core.iso_utc(scheduled.due_at_ms),
      schedule_core.iso_utc(scheduled.started_at_ms),
      scheduled.attempt,
      step_id,
      prepared,
      orchestrator,
    )
  case profile.driver {
    Some(driver) ->
      run_driver_lifecycle_or_create_directory(
        "driver_lifecycle_create",
        config_types.LifecycleCreate,
        driver,
        prepared,
        orchestrator,
        env,
      )
    None ->
      create_directory(prepared.path) |> result.map_error(WorkspaceFailure)
  }
}

fn run_scheduled_before_step_hook(
  scheduled: schedule_core.ScheduledRunContext,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, PrepareError) {
  let env =
    scheduled_hook_env(
      scheduled.job_id,
      schedule_core.iso_utc(scheduled.due_at_ms),
      schedule_core.iso_utc(scheduled.started_at_ms),
      scheduled.attempt,
      step_id,
      prepared,
      orchestrator,
    )
  case profile.driver {
    Some(driver) ->
      run_driver_lifecycle_if_supported(
        "driver_lifecycle_before_step",
        config_types.LifecycleBeforeStep,
        driver,
        prepared,
        orchestrator,
        env,
      )
    None -> Ok(Nil)
  }
}

fn run_driver_lifecycle_or_create_directory(
  name: String,
  operation: config_types.WorkspaceLifecycleOperation,
  driver: config_types.WorkspaceDriverConfig,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  env: List(#(String, String)),
) -> Result(Nil, PrepareError) {
  case workspace_driver_lifecycle.supports(driver, operation) {
    True ->
      workspace_driver_lifecycle.run(name, operation, driver, orchestrator, env)
      |> result.map_error(HookFailure)
    False ->
      create_directory(prepared.path) |> result.map_error(WorkspaceFailure)
  }
}

fn run_driver_lifecycle_if_supported(
  name: String,
  operation: config_types.WorkspaceLifecycleOperation,
  driver: config_types.WorkspaceDriverConfig,
  _prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  env: List(#(String, String)),
) -> Result(Nil, PrepareError) {
  workspace_driver_lifecycle.run_if_supported(
    name,
    operation,
    driver,
    orchestrator,
    env,
  )
  |> result.map_error(HookFailure)
}

fn run_remove_lifecycle(
  target_abs: String,
  orchestrator: config_types.OrchestratorConfig,
  profile: config_types.WorkspaceHookProfile,
) -> Result(Nil, error.WorkspaceError) {
  case profile.driver {
    Some(driver) ->
      workspace_driver_lifecycle.remove_run(
        target_abs,
        orchestrator,
        profile.name,
        driver,
      )
    None -> Ok(Nil)
  }
}

pub fn scheduled_hook_env(
  job_id: String,
  due_at: String,
  started_at: String,
  run_attempt: Int,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
) -> List(#(String, String)) {
  base_hook_env(step_id, prepared, orchestrator, "", "")
  |> append_scheduled_hook_env(job_id, due_at, started_at, run_attempt)
}

fn hook_env(
  issue: tracker_issue.Issue,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
) -> List(#(String, String)) {
  base_hook_env(step_id, prepared, orchestrator, issue.id, issue.identifier)
  |> append_issue_hook_env
}

fn base_hook_env(
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
  issue_id: String,
  issue_identifier: String,
) -> List(#(String, String)) {
  [
    #("SCHERZO_CONFIG_DIR", orchestrator.config_dir),
    #("SCHERZO_WORKFLOW_ID", prepared.workflow_id),
    #("SCHERZO_WORKFLOW_BUNDLE_DIR", prepared.workflow_bundle_dir),
    #("SCHERZO_RUN_ID", prepared.run_id),
    #("SCHERZO_RUN_ROOT", prepared.run_root),
    #("SCHERZO_ISSUE_ID", issue_id),
    #("SCHERZO_ISSUE_IDENTIFIER", issue_identifier),
    #("SCHERZO_STEP_ID", step_id),
    #("SCHERZO_ATTEMPT_INDEX", int.to_string(prepared.attempt_index)),
    #(
      "SCHERZO_ATTEMPT_KEY",
      workflow_identity.attempt_key(
        prepared.run_id,
        step_id,
        prepared.attempt_index,
      ),
    ),
    #(
      "SCHERZO_HOOK_IDEMPOTENCY_KEY",
      workflow_identity.hook_idempotency_key(prepared.run_id, step_id),
    ),
    #("SCHERZO_WORKSPACE_ROOT", orchestrator.effective.workspace.root),
    #("SCHERZO_WORKSPACE_PROFILE", prepared.workspace_profile),
    #("SCHERZO_WORKSPACE_NAME", prepared.workspace_name),
    #("SCHERZO_WORKSPACE_PATH", prepared.path),
    #(
      "SCHERZO_SOURCE_WORKSPACE_NAME",
      option.unwrap(prepared.source_workspace_name, ""),
    ),
    #(
      "SCHERZO_SOURCE_WORKSPACE_PATH",
      option.unwrap(prepared.source_workspace_path, ""),
    ),
  ]
}

fn append_issue_hook_env(
  env: List(#(String, String)),
) -> List(#(String, String)) {
  [#("SCHERZO_RUN_KIND", "issue"), ..env]
}

fn append_scheduled_hook_env(
  env: List(#(String, String)),
  job_id: String,
  due_at: String,
  started_at: String,
  run_attempt: Int,
) -> List(#(String, String)) {
  [
    #("SCHERZO_RUN_KIND", "scheduled"),
    #("SCHERZO_SCHEDULED_JOB_ID", job_id),
    #("SCHERZO_SCHEDULE_DUE_AT", due_at),
    #("SCHERZO_SCHEDULE_STARTED_AT", started_at),
    #("SCHERZO_RUN_ATTEMPT", int.to_string(run_attempt)),
    ..env
  ]
}

fn create_directory(path: String) -> Result(Nil, error.WorkspaceError) {
  simplifile.create_directory_all(path)
  |> result.replace_error(error.WorkspaceIo("create directory failed"))
}

fn ensure_directory_after_create(
  path: String,
) -> Result(Nil, error.WorkspaceError) {
  case simplifile.is_directory(path) {
    Ok(True) -> Ok(Nil)
    Ok(False) -> Error(error.PartialWorkspace(path))
    Error(_) -> Error(error.PartialWorkspace(path))
  }
}

fn try_workspace(
  result: Result(a, error.WorkspaceError),
  next: fn(a) -> Result(b, error.WorkspaceError),
) -> Result(b, error.WorkspaceError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}

fn try_prepare(
  result: Result(a, error.WorkspaceError),
  next: fn(a) -> Result(b, PrepareError),
) -> Result(b, PrepareError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(WorkspaceFailure(err))
  }
}
