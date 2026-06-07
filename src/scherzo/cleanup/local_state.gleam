import scherzo/state/local_artifacts

pub fn inventory(
  workspace_root: String,
  now_ms: Int,
) -> local_artifacts.CleanupResult {
  local_artifacts.inventory(workspace_root, now_ms, True)
}

pub fn apply(
  workspace_root: String,
  now_ms: Int,
) -> local_artifacts.CleanupResult {
  local_artifacts.apply_cleanup(workspace_root, now_ms)
}
