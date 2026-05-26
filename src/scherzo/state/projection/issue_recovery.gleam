import gleam/dict.{type Dict}
import gleam/list

pub const context_name = "issue_recovery"

pub fn known_workspace_for_issue(
  workspaces: Dict(String, workspace),
  issue_id: String,
  workspace_path_of: fn(workspace) -> String,
) -> Result(String, Nil) {
  case dict.get(workspaces, issue_id) {
    Ok(workspace) -> Ok(workspace_path_of(workspace))
    Error(Nil) -> Error(Nil)
  }
}

pub fn counter_has_source_run(
  counters: Dict(String, counter),
  issue_id: String,
  run_id: String,
  source_run_ids_of: fn(counter) -> List(String),
) -> Bool {
  case dict.get(counters, issue_id) {
    Ok(counter) -> list.contains(source_run_ids_of(counter), run_id)
    Error(Nil) -> False
  }
}

pub fn retry_due_at_ms(
  status: status,
  scheduled_retry: fn(status) -> Result(#(Int, Int), Nil),
) -> Result(Int, Nil) {
  case scheduled_retry(status) {
    Ok(#(delay_ms, scheduled_at_ms)) -> Ok(scheduled_at_ms + delay_ms)
    Error(Nil) -> Error(Nil)
  }
}
