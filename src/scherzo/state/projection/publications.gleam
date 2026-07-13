import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some, from_result}
import gleam/string

pub const context_name = "publications"

pub fn publication_key(run_id: String, publication_id: String) -> String {
  run_id <> ":" <> publication_id
}

pub fn series_key(series_id: String) -> String {
  series_id
}

pub fn append_attempt(
  attempts: Dict(String, List(attempt)),
  run_id: String,
  publication_id: String,
  attempt: attempt,
) -> Dict(String, List(attempt)) {
  let key = publication_key(run_id, publication_id)
  let existing = dict.get(attempts, key) |> from_result |> unwrap([])
  dict.insert(attempts, key, list.append(existing, [attempt]))
}

pub fn attempts_for(
  attempts: Dict(String, List(attempt)),
  run_id: String,
  publication_id: String,
) -> List(attempt) {
  dict.get(attempts, publication_key(run_id, publication_id))
  |> from_result
  |> unwrap([])
}

pub fn latest_for(
  attempts: Dict(String, List(attempt)),
  run_id: String,
  publication_id: String,
) -> Result(attempt, Nil) {
  case list.reverse(attempts_for(attempts, run_id, publication_id)) {
    [latest, ..] -> Ok(latest)
    [] -> Error(Nil)
  }
}

pub fn latest_for_series(
  latest_attempts: Dict(String, attempt),
  series_id: String,
) -> Result(attempt, Nil) {
  dict.get(latest_attempts, series_key(series_id))
}

pub fn rebuild_latest_by_series(
  existing_latest: Dict(String, attempt),
  attempts: Dict(String, List(attempt)),
  series_id_of: fn(attempt) -> String,
  recorded_at_ms_of: fn(attempt) -> Int,
  run_id_of: fn(attempt) -> String,
  publication_id_of: fn(attempt) -> String,
) -> Dict(String, attempt) {
  let preserved =
    dict.fold(existing_latest, dict.new(), fn(acc, series_id, attempt) {
      case attempt_present(attempts, attempt, run_id_of, publication_id_of) {
        True -> dict.insert(acc, series_id, attempt)
        False -> acc
      }
    })

  dict.values(attempts)
  |> list.flatten
  |> list.fold(preserved, fn(acc, attempt) {
    let series_id = series_id_of(attempt)
    case dict.get(acc, series_id) {
      Ok(existing) ->
        case recorded_at_ms_of(existing) > recorded_at_ms_of(attempt) {
          True -> acc
          False -> dict.insert(acc, series_id, attempt)
        }
      Error(Nil) -> dict.insert(acc, series_id, attempt)
    }
  })
}

fn attempt_present(
  attempts: Dict(String, List(attempt)),
  target: attempt,
  run_id_of: fn(attempt) -> String,
  publication_id_of: fn(attempt) -> String,
) -> Bool {
  case
    dict.get(
      attempts,
      publication_key(run_id_of(target), publication_id_of(target)),
    )
  {
    Ok(values) -> list.any(values, fn(attempt) { attempt == target })
    Error(Nil) -> False
  }
}

pub fn publication_ids_for_run(
  attempts: Dict(String, List(attempt)),
  run_id: String,
  publication_id_of: fn(attempt) -> String,
) -> List(String) {
  publication_ids_for_run_loop(
    dict.to_list(attempts),
    run_id,
    publication_id_of,
    [],
  )
}

fn publication_ids_for_run_loop(
  entries: List(#(String, List(attempt))),
  run_id: String,
  publication_id_of: fn(attempt) -> String,
  acc: List(String),
) -> List(String) {
  case entries {
    [] -> list.reverse(acc)
    [#(key, values), ..rest] ->
      case values, string.starts_with(key, publication_key_prefix(run_id)) {
        [first, ..], True ->
          publication_ids_for_run_loop(rest, run_id, publication_id_of, [
            publication_id_of(first),
            ..acc
          ])
        _, _ ->
          publication_ids_for_run_loop(rest, run_id, publication_id_of, acc)
      }
  }
}

fn publication_key_prefix(run_id: String) -> String {
  run_id <> ":"
}

fn unwrap(value: Option(a), default: a) -> a {
  case value {
    Some(value) -> value
    None -> default
  }
}
