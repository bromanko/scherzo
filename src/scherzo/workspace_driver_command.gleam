import gleam/option.{None, Some}
import gleam/result
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
    True -> path.dirname(config_dir) |> result.unwrap(config_dir)
    False -> config_dir
  }
}
