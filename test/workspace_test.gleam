import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/workspace
import simplifile

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

fn hooks(
  after_create: Option(String),
  before_run: Option(String),
) -> config_types.HooksConfig {
  config_types.HooksConfig(
    after_create: after_create,
    before_run: before_run,
    after_run: None,
    before_remove: None,
    timeout_ms: 2000,
  )
}

pub fn sanitize_identifiers_test() {
  let assert Ok("ABC-123") = workspace.sanitize("ABC-123")
  let assert Ok("A_B") = workspace.sanitize("A/B")
  let assert Ok("ABC_123") = workspace.sanitize("ABC 123")
  let assert Ok(".._outside") = workspace.sanitize("../outside")
  let assert Error(_) = workspace.sanitize(".")
  let assert Error(_) = workspace.sanitize("..")
  let assert Error(_) = workspace.sanitize("")
}

pub fn workspace_path_stays_under_root_test() {
  let root = "test/tmp/workspaces-root"
  reset_dir(root)
  let assert Ok(#(key, path)) = workspace.workspace_path(root, "A/B")
  assert key == "A_B"
  assert string.ends_with(path, "/test/tmp/workspaces-root/A_B")
}

pub fn create_reuse_and_file_collision_test() {
  let root = "test/tmp/workspace-create"
  reset_dir(root)
  let config = config_types.WorkspaceConfig(root: root)
  let hook_config = hooks(None, Some("test -d ."))

  let assert Ok(first) = workspace.prepare("ABC-123", config, hook_config)
  assert first.created == True
  let assert Ok(second) = workspace.prepare("ABC-123", config, hook_config)
  assert second.created == False

  let assert Ok(#(_, file_path)) = workspace.workspace_path(root, "FILE-1")
  let assert Ok(Nil) = simplifile.write(file_path, "not a directory")
  let assert Error(workspace.WorkspaceFailure(_)) =
    workspace.prepare("FILE-1", config, hook_config)
}

pub fn population_hook_runs_only_on_new_directory_test() {
  let root = "test/tmp/workspace-hooks"
  reset_dir(root)
  let config = config_types.WorkspaceConfig(root: root)
  let hook_config =
    hooks(Some("printf populated > POPULATED"), Some("test -f POPULATED"))

  let assert Ok(first) = workspace.prepare("ABC-123", config, hook_config)
  assert first.created == True
  let assert Ok(True) = simplifile.is_file(first.path <> "/POPULATED")

  let assert Ok(Nil) = simplifile.write(first.path <> "/POPULATED", "changed")
  let assert Ok(second) = workspace.prepare("ABC-123", config, hook_config)
  assert second.created == False
  let assert Ok(contents) = simplifile.read(second.path <> "/POPULATED")
  assert contents == "changed"
}

pub fn failing_after_create_removes_new_workspace_test() {
  let root = "test/tmp/workspace-failing-hook"
  reset_dir(root)
  let config = config_types.WorkspaceConfig(root: root)
  let hook_config = hooks(Some("printf partial > PARTIAL; exit 3"), None)

  let assert Error(workspace.HookFailure(_)) =
    workspace.prepare("ABC-123", config, hook_config)
  let assert Ok(#(_, path)) = workspace.workspace_path(root, "ABC-123")
  let assert Ok(False) = simplifile.is_directory(path)
}

pub fn sidecar_marker_forces_repopulation_test() {
  let root = "test/tmp/workspace-sidecar"
  reset_dir(root)
  let assert Ok(#(key, path)) = workspace.workspace_path(root, "ABC-123")
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  let assert Ok(Nil) = simplifile.write(path <> "/PARTIAL", "partial")
  let assert Ok(Nil) =
    simplifile.create_directory_all(root <> "/.scherzo-state")
  let assert Ok(Nil) =
    simplifile.write(
      root <> "/.scherzo-state/" <> key <> ".populating",
      "populating",
    )

  let config = config_types.WorkspaceConfig(root: root)
  let hook_config =
    hooks(Some("printf populated > POPULATED"), Some("test -f POPULATED"))
  let assert Ok(prepared) = workspace.prepare("ABC-123", config, hook_config)
  let assert Ok(True) = simplifile.is_file(prepared.path <> "/POPULATED")
  let assert Ok(False) = simplifile.is_file(prepared.path <> "/PARTIAL")
}

pub fn before_run_failure_aborts_attempt_and_after_run_is_best_effort_test() {
  let root = "test/tmp/workspace-before-run"
  reset_dir(root)
  let config = config_types.WorkspaceConfig(root: root)
  let hook_config = hooks(None, Some("exit 2"))
  let assert Error(workspace.HookFailure(_)) =
    workspace.prepare("ABC-123", config, hook_config)

  let log_line =
    workspace.after_run(
      root,
      config_types.HooksConfig(..hook_config, after_run: Some("exit 9")),
    )
  assert string.contains(log_line, "event=hook_failed")
}

pub fn cleanup_validates_stored_path_under_root_test() {
  let old_root = "test/tmp/workspace-old-root"
  let new_root = "test/tmp/workspace-new-root"
  reset_dir(old_root)
  reset_dir(new_root)
  let assert Ok(Nil) = simplifile.create_directory_all(old_root <> "/ABC-123")
  let assert Ok(Nil) = simplifile.write(old_root <> "/ABC-123/file", "old")
  let assert Ok(Nil) = simplifile.create_directory_all(new_root <> "/ABC-123")
  let assert Ok(Nil) = simplifile.write(new_root <> "/ABC-123/file", "new")

  let hook_config = hooks(None, None)
  let assert Error(_) =
    workspace.cleanup_stored_path(new_root, old_root <> "/ABC-123", hook_config)
  let assert Ok(True) = simplifile.is_directory(old_root <> "/ABC-123")
  let assert Ok(Nil) =
    workspace.cleanup_stored_path(old_root, old_root <> "/ABC-123", hook_config)
  let assert Ok(False) = simplifile.is_directory(old_root <> "/ABC-123")
  let assert Ok(True) = simplifile.is_directory(new_root <> "/ABC-123")

  let assert Error(_) = workspace.cleanup_stored_path(old_root, "", hook_config)
  let assert Error(_) =
    workspace.cleanup_stored_path(old_root, old_root, hook_config)
}
