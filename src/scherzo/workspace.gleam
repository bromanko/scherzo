import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/hooks
import scherzo/path
import scherzo/workspace_boundary as boundary
import simplifile

pub type PreparedWorkspace {
  PreparedWorkspace(key: String, path: String, created: Bool, populated: Bool)
}

pub type WorkspaceSource {
  FreshWorkspace
  DerivedWorkspace(name: String, path: String)
}

pub fn source_from_options(
  name: Option(String),
  path: Option(String),
) -> Result(WorkspaceSource, Nil) {
  case name, path {
    None, None -> Ok(FreshWorkspace)
    Some(name), Some(path) -> Ok(DerivedWorkspace(name: name, path: path))
    _, _ -> Error(Nil)
  }
}

pub fn source_to_options(
  source: WorkspaceSource,
) -> #(Option(String), Option(String)) {
  case source {
    FreshWorkspace -> #(None, None)
    DerivedWorkspace(name, path) -> #(Some(name), Some(path))
  }
}

pub fn source_name(source: WorkspaceSource) -> Option(String) {
  case source {
    FreshWorkspace -> None
    DerivedWorkspace(name, _) -> Some(name)
  }
}

pub fn source_path(source: WorkspaceSource) -> Option(String) {
  case source {
    FreshWorkspace -> None
    DerivedWorkspace(_, path) -> Some(path)
  }
}

pub type AfterRunOutcome {
  AfterRunSkipped
  AfterRunSucceeded(diagnostic: String)
  AfterRunFailed(diagnostic: String)
}

pub type PrepareError {
  WorkspaceFailure(error.WorkspaceError)
  HookFailure(error.HookError)
}

pub fn sanitize(identifier: String) -> Result(String, error.WorkspaceError) {
  let key =
    identifier
    |> string.to_graphemes
    |> list.map(sanitize_grapheme)
    |> string.join(with: "")
  case key {
    "" -> Error(error.UnsafeWorkspaceKey(identifier))
    "." -> Error(error.UnsafeWorkspaceKey(identifier))
    ".." -> Error(error.UnsafeWorkspaceKey(identifier))
    _ -> Ok(key)
  }
}

pub fn workspace_path(
  root: String,
  identifier: String,
) -> Result(#(String, String), error.WorkspaceError) {
  use key <- try_workspace(sanitize(identifier))
  use root_abs <- try_workspace(boundary.resolve_absolute_path(
    root,
    "workspace root",
  ))
  let joined = path.join(root_abs, key)
  use workspace_abs <- try_workspace(boundary.resolve_absolute_path(
    joined,
    "workspace path",
  ))
  case path.contains(root_abs, workspace_abs) {
    True -> Ok(#(key, workspace_abs))
    False -> Error(error.WorkspaceOutsideRoot(workspace_abs))
  }
}

pub fn prepare(
  identifier: String,
  workspace: config_types.WorkspaceConfig,
  hooks_config: config_types.HooksConfig,
) -> Result(PreparedWorkspace, PrepareError) {
  use key_and_path <- try_prepare_workspace(workspace_path(
    workspace.root,
    identifier,
  ))
  let #(key, workspace_path) = key_and_path
  use root_abs <- try_prepare_workspace(boundary.resolve_absolute_path(
    workspace.root,
    "workspace root",
  ))
  let marker = population_marker(root_abs, key)
  use marker_exists <- try_prepare_workspace(population_marker_exists(marker))
  use _ <- try_prepare_workspace(cleanup_stale_population(
    marker_exists,
    root_abs,
    workspace_path,
    marker,
  ))

  use created <- try_prepare_workspace(ensure_directory(workspace_path))
  let should_populate = created || marker_exists
  case should_populate && has_script(hooks_config.after_create) {
    True -> run_after_create(key, root_abs, workspace_path, hooks_config)
    False -> run_before_run(key, workspace_path, hooks_config, created, False)
  }
}

fn run_after_create(
  key: String,
  root_abs: String,
  workspace_path: String,
  hooks_config: config_types.HooksConfig,
) -> Result(PreparedWorkspace, PrepareError) {
  let marker = population_marker(root_abs, key)
  case hooks_config.after_create {
    Some(script) -> {
      case mark_population_started(root_abs, marker) {
        Error(err) ->
          fail_population_start_with_cleanup(
            err,
            root_abs,
            workspace_path,
            marker,
          )
        Ok(Nil) ->
          case
            hooks.run_hook(
              "after_create",
              script,
              workspace_path,
              hooks_config.timeout_ms,
            )
          {
            Ok(Nil) -> {
              use _ <- try_prepare_workspace(delete_population_marker(marker))
              run_before_run(key, workspace_path, hooks_config, True, True)
            }
            Error(err) ->
              fail_after_create_with_cleanup(
                err,
                root_abs,
                workspace_path,
                marker,
              )
          }
      }
    }
    None -> run_before_run(key, workspace_path, hooks_config, True, False)
  }
}

fn run_before_run(
  key: String,
  workspace_path: String,
  hooks_config: config_types.HooksConfig,
  created: Bool,
  populated: Bool,
) -> Result(PreparedWorkspace, PrepareError) {
  case hooks_config.before_run {
    Some(script) ->
      case
        hooks.run_hook(
          "before_run",
          script,
          workspace_path,
          hooks_config.timeout_ms,
        )
      {
        Ok(Nil) ->
          Ok(PreparedWorkspace(
            key: key,
            path: workspace_path,
            created: created,
            populated: populated,
          ))
        Error(err) -> Error(HookFailure(err))
      }
    None ->
      Ok(PreparedWorkspace(
        key: key,
        path: workspace_path,
        created: created,
        populated: populated,
      ))
  }
}

pub fn after_run(
  workspace_path: String,
  hooks_config: config_types.HooksConfig,
) -> AfterRunOutcome {
  case hooks_config.after_run {
    Some(script) ->
      case
        hooks.run_best_effort_outcome(
          "after_run",
          script,
          workspace_path,
          hooks_config.timeout_ms,
        )
      {
        hooks.BestEffortHookSucceeded(diagnostic) ->
          AfterRunSucceeded(diagnostic)
        hooks.BestEffortHookFailed(diagnostic) -> AfterRunFailed(diagnostic)
      }
    None -> AfterRunSkipped
  }
}

pub fn cleanup(
  workspace_root: String,
  workspace_path: String,
  hooks_config: config_types.HooksConfig,
) -> Result(Nil, error.WorkspaceError) {
  safe_cleanup(workspace_root, workspace_path, hooks_config)
}

pub fn cleanup_stored_path(
  workspace_root: String,
  stored_workspace_path: String,
  hooks_config: config_types.HooksConfig,
) -> Result(Nil, error.WorkspaceError) {
  safe_cleanup(workspace_root, stored_workspace_path, hooks_config)
}

fn ensure_directory(
  workspace_path: String,
) -> Result(Bool, error.WorkspaceError) {
  case simplifile.is_directory(workspace_path) {
    Ok(True) -> Ok(False)
    Ok(False) ->
      case simplifile.is_file(workspace_path) {
        Ok(True) -> Error(error.WorkspaceCollision(workspace_path))
        Ok(False) | Error(simplifile.Enoent) -> create_directory(workspace_path)
        Error(file_error) ->
          Error(error.WorkspaceIo(
            "stat workspace file failed: "
            <> simplifile.describe_error(file_error),
          ))
      }
    Error(simplifile.Enoent) -> create_directory(workspace_path)
    Error(file_error) ->
      Error(error.WorkspaceIo(
        "stat workspace directory failed: "
        <> simplifile.describe_error(file_error),
      ))
  }
}

fn create_directory(
  workspace_path: String,
) -> Result(Bool, error.WorkspaceError) {
  case simplifile.create_directory_all(workspace_path) {
    Ok(Nil) -> Ok(True)
    Error(file_error) ->
      Error(error.WorkspaceIo(
        "create workspace directory failed: "
        <> simplifile.describe_error(file_error),
      ))
  }
}

fn safe_cleanup(
  workspace_root: String,
  workspace_path: String,
  hooks_config: config_types.HooksConfig,
) -> Result(Nil, error.WorkspaceError) {
  case string.trim(workspace_path) == "" {
    True -> Error(error.WorkspaceOutsideRoot(workspace_path))
    False -> {
      use root_abs <- try_workspace(boundary.resolve_absolute_path(
        workspace_root,
        "workspace root",
      ))
      use target_abs <- try_workspace(boundary.resolve_absolute_path(
        workspace_path,
        "workspace cleanup target",
      ))
      case path.contains(root_abs, target_abs) && target_abs != root_abs {
        False -> Error(error.WorkspaceOutsideRoot(target_abs))
        True -> {
          case hooks_config.before_remove {
            Some(script) ->
              run_before_remove_best_effort(
                script,
                target_abs,
                hooks_config.timeout_ms,
              )
            None -> Nil
          }
          delete_path_if_present(target_abs)
        }
      }
    }
  }
}

fn safe_delete(
  root_abs: String,
  target: String,
) -> Result(Nil, error.WorkspaceError) {
  use target_abs <- try_workspace(boundary.resolve_absolute_path(
    target,
    "workspace cleanup target",
  ))
  case path.contains(root_abs, target_abs) {
    False -> Error(error.WorkspaceOutsideRoot(target_abs))
    True -> delete_path_if_present(target_abs)
  }
}

fn cleanup_stale_population(
  marker_exists: Bool,
  root_abs: String,
  workspace_path: String,
  marker: String,
) -> Result(Nil, error.WorkspaceError) {
  case marker_exists {
    True -> cleanup_failed_population(root_abs, workspace_path, marker)
    False -> Ok(Nil)
  }
}

fn mark_population_started(
  root_abs: String,
  marker: String,
) -> Result(Nil, error.WorkspaceError) {
  use _ <- result.try(create_state_directory(root_abs))
  case simplifile.write(marker, "populating") {
    Ok(Nil) -> Ok(Nil)
    Error(file_error) ->
      Error(error.WorkspaceIo(
        "write population marker failed: "
        <> simplifile.describe_error(file_error),
      ))
  }
}

fn create_state_directory(
  root_abs: String,
) -> Result(Nil, error.WorkspaceError) {
  case simplifile.create_directory_all(path.join(root_abs, ".scherzo-state")) {
    Ok(Nil) -> Ok(Nil)
    Error(file_error) ->
      Error(error.WorkspaceIo(
        "create workspace state directory failed: "
        <> simplifile.describe_error(file_error),
      ))
  }
}

fn delete_population_marker(
  marker: String,
) -> Result(Nil, error.WorkspaceError) {
  case simplifile.delete_file(at: marker) {
    Ok(Nil) -> Ok(Nil)
    Error(simplifile.Enoent) -> Ok(Nil)
    Error(file_error) ->
      Error(error.WorkspaceIo(
        "delete population marker failed: "
        <> simplifile.describe_error(file_error),
      ))
  }
}

fn fail_population_start_with_cleanup(
  start_error: error.WorkspaceError,
  root_abs: String,
  workspace_path: String,
  marker: String,
) -> Result(PreparedWorkspace, PrepareError) {
  case cleanup_failed_population(root_abs, workspace_path, marker) {
    Ok(Nil) -> Error(WorkspaceFailure(start_error))
    Error(cleanup_error) ->
      Error(
        WorkspaceFailure(error.WorkspaceIo(
          "start population failed and cleanup failed: "
          <> boundary.workspace_error_summary(start_error)
          <> "; cleanup: "
          <> boundary.workspace_error_summary(cleanup_error),
        )),
      )
  }
}

fn fail_after_create_with_cleanup(
  hook_error: error.HookError,
  root_abs: String,
  workspace_path: String,
  marker: String,
) -> Result(PreparedWorkspace, PrepareError) {
  case cleanup_failed_population(root_abs, workspace_path, marker) {
    Ok(Nil) -> Error(HookFailure(hook_error))
    Error(cleanup_error) ->
      Error(
        WorkspaceFailure(error.WorkspaceIo(
          "after_create failed and cleanup failed: "
          <> boundary.hook_error_summary(hook_error)
          <> "; cleanup: "
          <> boundary.workspace_error_summary(cleanup_error),
        )),
      )
  }
}

fn cleanup_failed_population(
  root_abs: String,
  workspace_path: String,
  marker: String,
) -> Result(Nil, error.WorkspaceError) {
  use _ <- result.try(safe_delete(root_abs, workspace_path))
  delete_population_marker(marker)
}

fn run_before_remove_best_effort(
  script: String,
  target_abs: String,
  timeout_ms: Int,
) -> Nil {
  let operator_visible_log =
    hooks.run_best_effort("before_remove", script, target_abs, timeout_ms)
  acknowledge_best_effort_log(operator_visible_log)
}

fn acknowledge_best_effort_log(_operator_visible_log: String) -> Nil {
  Nil
}

fn delete_path_if_present(
  target_abs: String,
) -> Result(Nil, error.WorkspaceError) {
  case simplifile.delete(target_abs) {
    Ok(Nil) -> Ok(Nil)
    Error(simplifile.Enoent) -> Ok(Nil)
    Error(file_error) ->
      Error(error.WorkspaceIo(
        "delete workspace path failed: "
        <> simplifile.describe_error(file_error),
      ))
  }
}

fn population_marker(root_abs: String, key: String) -> String {
  path.join(path.join(root_abs, ".scherzo-state"), key <> ".populating")
}

fn population_marker_exists(
  file: String,
) -> Result(Bool, error.WorkspaceError) {
  case simplifile.is_file(file) {
    Ok(True) -> Ok(True)
    Ok(False) | Error(simplifile.Enoent) -> Ok(False)
    Error(file_error) ->
      Error(error.WorkspaceIo(
        "inspect population marker failed: "
        <> simplifile.describe_error(file_error),
      ))
  }
}

fn has_script(script: Option(String)) -> Bool {
  case script {
    Some(script) -> string.trim(script) != ""
    None -> False
  }
}

fn sanitize_grapheme(grapheme: String) -> String {
  case is_allowed(grapheme) {
    True -> grapheme
    False -> "_"
  }
}

fn is_allowed(grapheme: String) -> Bool {
  is_between(grapheme, "A", "Z")
  || is_between(grapheme, "a", "z")
  || is_between(grapheme, "0", "9")
  || grapheme == "."
  || grapheme == "_"
  || grapheme == "-"
}

fn is_between(value: String, low: String, high: String) -> Bool {
  string.compare(value, low) != Lt && string.compare(value, high) != Gt
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

fn try_prepare_workspace(
  result: Result(a, error.WorkspaceError),
  next: fn(a) -> Result(b, PrepareError),
) -> Result(b, PrepareError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(WorkspaceFailure(err))
  }
}
