import gleam/dict.{type Dict}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/hooks
import scherzo/path
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workflow_identity
import scherzo/workspace
import simplifile

pub type PreparedStepWorkspace {
  PreparedStepWorkspace(
    workflow_id: String,
    run_id: String,
    run_root: String,
    attempt_index: Int,
    workspace_name: String,
    path: String,
    source_workspace_name: Option(String),
    source_workspace_path: Option(String),
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
        orchestrator,
      ))
      reuse_prepared_workspace(
        issue,
        step_id,
        prepared,
        attempt_index,
        orchestrator,
      )
    }
    None -> {
      use _ <- try_prepare(validate_recovered_source_workspace(
        workspace_ref.from,
        known_workspaces,
        workflow_id,
        run_id,
        expected_run_root,
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
      ))
      let #(source_name, source_path) = source
      use _ <- try_prepare(validate_source_directory(source_path))
      use _ <- try_prepare(create_directory(run_root))
      let prepared =
        PreparedStepWorkspace(
          workflow_id: workflow_id,
          run_id: run_id,
          run_root: run_root,
          attempt_index: attempt_index,
          workspace_name: workspace_ref.name,
          path: workspace_path,
          source_workspace_name: source_name,
          source_workspace_path: source_path,
        )
      case finish_prepare_step(issue, step_id, prepared, orchestrator) {
        Ok(prepared) -> Ok(prepared)
        Error(err) -> {
          case cleanup_on_error {
            True -> {
              let _ = cleanup_run(run_root, orchestrator)
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
) -> Result(PreparedStepWorkspace, PrepareError) {
  use _ <- result_try(run_create_hook(issue, step_id, prepared, orchestrator))
  use _ <- try_prepare(ensure_directory_after_create(prepared.path))
  use _ <- result_try(run_before_step_hook(
    issue,
    step_id,
    prepared,
    orchestrator,
  ))
  Ok(prepared)
}

pub fn after_step(
  issue: tracker_issue.Issue,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
) -> Nil {
  case orchestrator.dag_hooks.after_step {
    None -> Nil
    Some(script) -> {
      let _ =
        hooks.run_best_effort_with_env(
          "after_step",
          script,
          orchestrator.config_dir,
          orchestrator.dag_hooks.timeout_ms,
          hook_env(issue, step_id, prepared, orchestrator),
        )
      Nil
    }
  }
}

pub fn cleanup_run(
  run_root: String,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(Nil, error.WorkspaceError) {
  let root_abs =
    path.absolute(orchestrator.effective.workspace.root)
    |> result_unwrap(orchestrator.effective.workspace.root)
  let target_abs = path.absolute(run_root) |> result_unwrap(run_root)
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
          case orchestrator.dag_hooks.remove {
            None -> Nil
            Some(script) -> {
              let dummy_issue =
                tracker_issue.Issue(
                  id: "",
                  identifier: "",
                  title: "",
                  description: None,
                  priority: None,
                  state: issue_state.from_string_unchecked(""),
                  branch_name: None,
                  url: None,
                  labels: [],
                  blocked_by: [],
                  created_at: None,
                  updated_at: None,
                )
              let prepared =
                PreparedStepWorkspace(
                  workflow_id: "",
                  run_id: "",
                  run_root: target_abs,
                  attempt_index: 0,
                  workspace_name: "",
                  path: target_abs,
                  source_workspace_name: None,
                  source_workspace_path: None,
                )
              let _ =
                hooks.run_best_effort_with_env(
                  "remove",
                  script,
                  orchestrator.config_dir,
                  orchestrator.dag_hooks.timeout_ms,
                  hook_env(dummy_issue, "", prepared, orchestrator),
                )
              Nil
            }
          }
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
    |> result_unwrap(orchestrator.effective.workspace.root)
  let issue_root = path.join(path.join(root_abs, workflow_key), issue_key)
  let run_root = path.join(issue_root, run_key)
  let run_root_abs = path.absolute(run_root) |> result_unwrap(run_root)
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
    path.absolute(expected_run_root) |> result_unwrap(expected_run_root)
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
  orchestrator: config_types.OrchestratorConfig,
) -> Result(Nil, error.WorkspaceError) {
  let expected_abs =
    path.absolute(expected_run_root) |> result_unwrap(expected_run_root)
  let prepared_run_root_abs =
    path.absolute(prepared.run_root) |> result_unwrap(prepared.run_root)
  let root_abs =
    path.absolute(orchestrator.effective.workspace.root)
    |> result_unwrap(orchestrator.effective.workspace.root)
  let prepared_path_abs =
    path.absolute(prepared.path) |> result_unwrap(prepared.path)
  case
    prepared.workflow_id == workflow_id
    && prepared.run_id == run_id
    && prepared.workspace_name == workspace_name
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
    |> result_unwrap(orchestrator.effective.workspace.root)
  let issue_root = path.join(path.join(root_abs, workflow_key), issue_key)
  let run_root = path.join(issue_root, run_key)
  let workspace_path =
    path.join(path.join(run_root, "workspaces"), workspace_key)
  let run_root_abs = path.absolute(run_root) |> result_unwrap(run_root)
  let workspace_abs =
    path.absolute(workspace_path) |> result_unwrap(workspace_path)
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
) -> Result(PreparedStepWorkspace, PrepareError) {
  let prepared =
    PreparedStepWorkspace(
      ..prepared,
      attempt_index: attempt_index,
      source_workspace_name: Some(prepared.workspace_name),
      source_workspace_path: Some(prepared.path),
    )
  use _ <- try_prepare(ensure_directory_after_create(prepared.path))
  use _ <- result_try(run_before_step_hook(
    issue,
    step_id,
    prepared,
    orchestrator,
  ))
  Ok(prepared)
}

fn source_workspace(
  from: Option(String),
  known_workspaces: Dict(String, PreparedStepWorkspace),
) -> Result(#(Option(String), Option(String)), error.WorkspaceError) {
  case from {
    None -> Ok(#(None, None))
    Some(name) ->
      case dict.get(known_workspaces, name) {
        Ok(prepared) -> Ok(#(Some(name), Some(prepared.path)))
        Error(_) ->
          Error(error.WorkspaceIo("source workspace is not prepared: " <> name))
      }
  }
}

fn run_create_hook(
  issue: tracker_issue.Issue,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(Nil, PrepareError) {
  case orchestrator.dag_hooks.create {
    None ->
      create_directory(prepared.path) |> result_map_error(WorkspaceFailure)
    Some(script) ->
      hooks.run_hook_with_env(
        "create",
        script,
        orchestrator.config_dir,
        orchestrator.dag_hooks.timeout_ms,
        hook_env(issue, step_id, prepared, orchestrator),
      )
      |> result_map_error(HookFailure)
  }
}

fn run_before_step_hook(
  issue: tracker_issue.Issue,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
) -> Result(Nil, PrepareError) {
  case orchestrator.dag_hooks.before_step {
    None -> Ok(Nil)
    Some(script) ->
      hooks.run_hook_with_env(
        "before_step",
        script,
        orchestrator.config_dir,
        orchestrator.dag_hooks.timeout_ms,
        hook_env(issue, step_id, prepared, orchestrator),
      )
      |> result_map_error(HookFailure)
  }
}

fn hook_env(
  issue: tracker_issue.Issue,
  step_id: String,
  prepared: PreparedStepWorkspace,
  orchestrator: config_types.OrchestratorConfig,
) -> List(#(String, String)) {
  [
    #("SCHERZO_CONFIG_DIR", orchestrator.config_dir),
    #("SCHERZO_WORKFLOW_ID", prepared.workflow_id),
    #("SCHERZO_RUN_ID", prepared.run_id),
    #("SCHERZO_RUN_ROOT", prepared.run_root),
    #("SCHERZO_ISSUE_ID", issue.id),
    #("SCHERZO_ISSUE_IDENTIFIER", issue.identifier),
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
    #("SCHERZO_WORKSPACE_NAME", prepared.workspace_name),
    #("SCHERZO_WORKSPACE_PATH", prepared.path),
    #(
      "SCHERZO_SOURCE_WORKSPACE_NAME",
      option_unwrap(prepared.source_workspace_name, ""),
    ),
    #(
      "SCHERZO_SOURCE_WORKSPACE_PATH",
      option_unwrap(prepared.source_workspace_path, ""),
    ),
  ]
}

fn create_directory(path: String) -> Result(Nil, error.WorkspaceError) {
  simplifile.create_directory_all(path)
  |> result_map_error(fn(_) { error.WorkspaceIo("create directory failed") })
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

fn option_unwrap(value: Option(a), default: a) -> a {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn result_unwrap(result: Result(a, b), default: a) -> a {
  case result {
    Ok(value) -> value
    Error(_) -> default
  }
}

fn result_map_error(result: Result(a, e), mapper: fn(e) -> f) -> Result(a, f) {
  case result {
    Ok(value) -> Ok(value)
    Error(err) -> Error(mapper(err))
  }
}

fn result_try(
  result: Result(a, e),
  next: fn(a) -> Result(b, e),
) -> Result(b, e) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
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
