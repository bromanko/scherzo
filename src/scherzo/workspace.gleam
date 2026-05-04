import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/hooks
import scherzo/path
import simplifile

pub type PreparedWorkspace {
  PreparedWorkspace(key: String, path: String, created: Bool, populated: Bool)
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
  let root_abs = path.absolute(root) |> result_unwrap(root)
  let joined = path.join(root_abs, key)
  let workspace_abs = path.absolute(joined) |> result_unwrap(joined)
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
  let root_abs = path.absolute(workspace.root) |> result_unwrap(workspace.root)
  let marker = population_marker(root_abs, key)
  let marker_exists = file_exists(marker)

  case marker_exists {
    True -> {
      let _ = safe_delete(root_abs, workspace_path)
      Nil
    }
    False -> Nil
  }

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
  let _ = simplifile.create_directory_all(path.join(root_abs, ".scherzo-state"))
  let _ = simplifile.write(marker, "populating")
  case hooks_config.after_create {
    Some(script) ->
      case
        hooks.run_hook(
          "after_create",
          script,
          workspace_path,
          hooks_config.timeout_ms,
        )
      {
        Ok(Nil) -> {
          let _ = simplifile.delete_file(at: marker)
          run_before_run(key, workspace_path, hooks_config, True, True)
        }
        Error(err) -> {
          let cleanup = safe_delete(root_abs, workspace_path)
          case cleanup {
            Ok(Nil) -> {
              let _ = simplifile.delete_file(at: marker)
              Nil
            }
            Error(_) -> Nil
          }
          Error(HookFailure(err))
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
) -> String {
  case hooks_config.after_run {
    Some(script) ->
      hooks.run_best_effort(
        "after_run",
        script,
        workspace_path,
        hooks_config.timeout_ms,
      )
    None -> ""
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
        _ -> create_directory(workspace_path)
      }
    Error(simplifile.Enoent) -> create_directory(workspace_path)
    Error(_) -> Error(error.WorkspaceIo("stat directory failed"))
  }
}

fn create_directory(
  workspace_path: String,
) -> Result(Bool, error.WorkspaceError) {
  case simplifile.create_directory_all(workspace_path) {
    Ok(Nil) -> Ok(True)
    Error(_) -> Error(error.WorkspaceIo("create directory failed"))
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
      let root_abs =
        path.absolute(workspace_root) |> result_unwrap(workspace_root)
      let target_abs =
        path.absolute(workspace_path) |> result_unwrap(workspace_path)
      case path.contains(root_abs, target_abs) && target_abs != root_abs {
        False -> Error(error.WorkspaceOutsideRoot(target_abs))
        True -> {
          case hooks_config.before_remove {
            Some(script) -> {
              let _ =
                hooks.run_best_effort(
                  "before_remove",
                  script,
                  target_abs,
                  hooks_config.timeout_ms,
                )
              Nil
            }
            None -> Nil
          }
          simplifile.delete(target_abs)
          |> result_map_error(fn(_) { error.WorkspaceIo("delete failed") })
        }
      }
    }
  }
}

fn safe_delete(
  root_abs: String,
  target: String,
) -> Result(Nil, error.WorkspaceError) {
  let target_abs = path.absolute(target) |> result_unwrap(target)
  case path.contains(root_abs, target_abs) {
    False -> Error(error.WorkspaceOutsideRoot(target_abs))
    True ->
      simplifile.delete(target_abs)
      |> result_map_error(fn(_) { error.WorkspaceIo("delete failed") })
  }
}

fn population_marker(root_abs: String, key: String) -> String {
  path.join(path.join(root_abs, ".scherzo-state"), key <> ".populating")
}

fn file_exists(file: String) -> Bool {
  case simplifile.is_file(file) {
    Ok(True) -> True
    _ -> False
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
