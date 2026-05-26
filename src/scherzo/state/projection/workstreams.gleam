import gleam/dict.{type Dict}

pub const context_name = "workstreams"

pub fn update_status(
  statuses: Dict(String, status),
  workstream_id: String,
  empty_status: fn(String) -> status,
  update: fn(status) -> status,
) -> Dict(String, status) {
  let current = case dict.get(statuses, workstream_id) {
    Ok(status) -> status
    Error(Nil) -> empty_status(workstream_id)
  }

  dict.insert(statuses, workstream_id, update(current))
}
