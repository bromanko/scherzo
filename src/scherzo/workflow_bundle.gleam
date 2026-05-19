import gleam/dict
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/path

pub fn dir(
  orchestrator: config_types.OrchestratorConfig,
  workflow_id: String,
) -> String {
  case dict.get(orchestrator.routing.workflows, workflow_id) {
    Ok(workflow_path) ->
      bundle_dir_for_path(orchestrator.config_dir, workflow_path)
    Error(_) -> ""
  }
}

pub fn bundle_dir_for_path(
  config_dir: String,
  workflow_path: String,
) -> String {
  let path_text = case string.starts_with(workflow_path, "/") {
    True -> workflow_path
    False -> path.join(config_dir, workflow_path)
  }
  let absolute = path.absolute(path_text) |> result.unwrap(path_text)
  let canonical = path.realpath(absolute) |> result.unwrap(absolute)
  path.dirname(canonical) |> result.unwrap(canonical)
}
