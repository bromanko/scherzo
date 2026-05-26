import gleam/dict.{type Dict}
import gleam/list

pub const context_name = "legacy_runs"

pub fn started(
  statuses: Dict(String, status),
  run_id: String,
  status: status,
) -> Dict(String, status) {
  dict.insert(statuses, run_id, status)
}

pub fn finished(
  statuses: Dict(String, status),
  run_id: String,
  status: status,
) -> Dict(String, status) {
  dict.insert(statuses, run_id, status)
}

pub fn interrupted(
  statuses: Dict(String, status),
  run_id: String,
  status: status,
) -> Dict(String, status) {
  dict.insert(statuses, run_id, status)
}

pub fn issue_ids(
  statuses: Dict(String, status),
  issue_id_of: fn(status) -> String,
) -> List(String) {
  statuses |> dict.values |> list.map(issue_id_of)
}
