import gleam/int
import gleam/list
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import scherzo/hash

pub fn safe_component(value: String, fallback: String) -> String {
  let component =
    value
    |> string.to_graphemes
    |> list.map(sanitize_grapheme)
    |> string.join(with: "")
    |> string.trim
  case component {
    "" -> fallback
    "." -> fallback
    ".." -> fallback
    _ -> component
  }
}

pub fn step_hash(step_id: String) -> String {
  hash.short_sha256_hex(step_id, 12)
}

pub fn step_component(step_id: String) -> String {
  safe_component(step_id, "step") <> "-" <> step_hash(step_id)
}

pub fn step_session_id(
  run_id: String,
  step_id: String,
  attempt_index: Int,
) -> String {
  "workflow-step-"
  <> safe_component(run_id, "run")
  <> "-"
  <> safe_component(step_id, "step")
  <> "-a"
  <> int.to_string(attempt_index)
  <> "-"
  <> step_hash(step_id)
}

pub fn attempt_key(
  run_id: String,
  step_id: String,
  attempt_index: Int,
) -> String {
  run_id <> ":" <> step_id <> ":" <> int.to_string(attempt_index)
}

pub fn hook_idempotency_key(run_id: String, step_id: String) -> String {
  run_id <> ":" <> step_id
}

pub fn attempt_index_from_path(path: String) -> Result(Int, Nil) {
  path
  |> string.split(on: "/")
  |> list.reverse
  |> find_attempt_segment
}

fn find_attempt_segment(segments: List(String)) -> Result(Int, Nil) {
  case segments {
    [] -> Error(Nil)
    [segment, ..rest] ->
      case string.starts_with(segment, "attempt-") {
        True ->
          segment
          |> string.drop_start(8)
          |> int.parse
          |> result.replace_error(Nil)
        False -> find_attempt_segment(rest)
      }
  }
}

fn sanitize_grapheme(grapheme: String) -> String {
  case is_allowed(grapheme) {
    True -> grapheme
    False -> "_"
  }
}

fn is_allowed(grapheme: String) -> Bool {
  is_between(grapheme, "A", "Z")
  || is_between(grapheme, "a", "z")
  || is_between(grapheme, "0", "9")
  || grapheme == "."
  || grapheme == "_"
  || grapheme == "-"
}

fn is_between(value: String, low: String, high: String) -> Bool {
  string.compare(value, low) != Lt && string.compare(value, high) != Gt
}
