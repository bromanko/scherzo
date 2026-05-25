import gleam/option.{None, Some}
import gleam/string
import scherzo/config/types as config_types
import scherzo/path

const repo_root_placeholder = "$SCHERZO_REPO_ROOT"

pub fn resolve(
  command: String,
  orchestrator: config_types.OrchestratorConfig,
) -> String {
  case command == repo_root_placeholder {
    True -> default_repo_root(orchestrator)
    False ->
      case string.starts_with(command, repo_root_placeholder <> "/") {
        True ->
          default_repo_root(orchestrator)
          <> string.drop_start(command, string.length(repo_root_placeholder))
        False -> command
      }
  }
}

pub fn default_repo_root(
  orchestrator: config_types.OrchestratorConfig,
) -> String {
  case path.env("SCHERZO_REPO_ROOT") {
    Some(root) -> root
    None -> inferred_repo_root(orchestrator.config_dir)
  }
}

pub fn inferred_repo_root(config_dir: String) -> String {
  case string.ends_with(config_dir, "/.scherzo") {
    True -> stable_parent_or_config_dir(config_dir)
    False -> config_dir
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
