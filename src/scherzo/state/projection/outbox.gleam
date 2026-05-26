import gleam/dict.{type Dict}
import gleam/list

pub const context_name = "outbox"

pub fn insert_status(
  statuses: Dict(String, status),
  outbox_id: String,
  status: status,
) -> Dict(String, status) {
  dict.insert(statuses, outbox_id, status)
}

pub fn pending_replays(
  statuses: Dict(String, status),
  to_pending_replay: fn(#(String, status)) -> Result(replay, error),
) -> Result(List(replay), error) {
  pending_replays_from_entries(dict.to_list(statuses), to_pending_replay)
}

pub fn pending_replays_from_entries(
  entries: List(#(String, status)),
  to_pending_replay: fn(#(String, status)) -> Result(replay, error),
) -> Result(List(replay), error) {
  pending_replays_loop(entries, [], to_pending_replay)
}

fn pending_replays_loop(
  entries: List(#(String, status)),
  replays: List(replay),
  to_pending_replay: fn(#(String, status)) -> Result(replay, error),
) -> Result(List(replay), error) {
  case entries {
    [] -> Ok(list.reverse(replays))
    [entry, ..rest] ->
      case to_pending_replay(entry) {
        Ok(replay) ->
          pending_replays_loop(rest, [replay, ..replays], to_pending_replay)
        Error(error) -> Error(error)
      }
  }
}
