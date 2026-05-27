import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/path
import simplifile

const repo_root_placeholder = "$SCHERZO_REPO_ROOT"

const bundled_noop_driver_path = "scripts/scherzo-workspace-noop"

const bundled_jj_driver_path = "scripts/scherzo-workspace-jj"

const packaged_noop_driver_command = "scherzo-workspace-noop"

const packaged_jj_driver_command = "scherzo-workspace-jj"

pub fn resolve(
  command: String,
  orchestrator: config_types.OrchestratorConfig,
) -> String {
  case command == repo_root_placeholder {
    True -> default_repo_root(orchestrator)
    False ->
      case string.starts_with(command, repo_root_placeholder <> "/") {
        True ->
          resolve_repo_root_command(
            command,
            string.drop_start(command, string.length(repo_root_placeholder)),
            orchestrator,
          )
        False -> command
      }
  }
}

fn resolve_repo_root_command(
  command: String,
  suffix: String,
  orchestrator: config_types.OrchestratorConfig,
) -> String {
  let resolved = default_repo_root(orchestrator) <> suffix
  case packaged_builtin_driver_command(command, resolved) {
    Some(fallback) -> fallback
    None -> resolved
  }
}

fn packaged_builtin_driver_command(
  command: String,
  resolved: String,
) -> Option(String) {
  case is_file(resolved) {
    True -> None
    False ->
      case command == repo_root_placeholder <> "/" <> bundled_noop_driver_path {
        True -> Some(packaged_noop_driver_command)
        False ->
          case
            command == repo_root_placeholder <> "/" <> bundled_jj_driver_path
          {
            True -> Some(packaged_jj_driver_command)
            False -> None
          }
      }
  }
}

pub fn default_repo_root(
  orchestrator: config_types.OrchestratorConfig,
) -> String {
  case path.env("SCHERZO_REPO_ROOT") {
    Some(root) -> root
    None -> default_repo_root_for_config_dir(orchestrator.config_dir)
  }
}

pub fn inferred_repo_root(config_dir: String) -> String {
  case string.ends_with(config_dir, "/.scherzo") {
    True -> stable_parent_or_config_dir(config_dir)
    False -> config_dir
  }
}

fn default_repo_root_for_config_dir(config_dir: String) -> String {
  let conventional_root = inferred_repo_root(config_dir)
  case find_bundled_driver_root(conventional_root) {
    Some(root) -> root
    None -> conventional_root
  }
}

// Minimal test and ad-hoc configs often live below the repository root. When
// SCHERZO_REPO_ROOT is unset, climb to the nearest source tree that contains the
// bundled no-op driver so built-in driver commands still resolve to an executable.
fn find_bundled_driver_root(start: String) -> Option(String) {
  case has_bundled_noop_driver(start) {
    True -> Some(start)
    False ->
      case path.dirname(start) {
        Ok(parent) if parent != start -> find_bundled_driver_root(parent)
        Ok(_) -> None
        Error(_error) -> None
      }
  }
}

fn has_bundled_noop_driver(root: String) -> Bool {
  is_file(path.join(root, bundled_noop_driver_path))
}

fn is_file(path: String) -> Bool {
  case simplifile.is_file(path) {
    Ok(True) -> True
    Ok(False) -> False
    Error(_error) -> False
  }
}

fn stable_parent_or_config_dir(config_dir: String) -> String {
  case path.dirname(config_dir) {
    Ok(parent) -> parent
    // `path.dirname` is backed by an infallible runtime path helper today; if
    // that boundary ever reports failure, keep the historical stable default
    // rather than inventing a repo root outside the configured directory.
    Error(Nil) -> config_dir
  }
}
