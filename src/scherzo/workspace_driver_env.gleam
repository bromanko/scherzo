import gleam/list
import gleam/order.{Gt, Lt}
import gleam/string
import scherzo/hash

pub fn valid_key(name: String) -> Bool {
  case string.to_graphemes(name) {
    [] -> False
    [first, ..rest] ->
      { is_alpha(first) || first == "_" } && all(rest, is_key_tail)
  }
}

pub fn reserved_generated_key(name: String) -> Bool {
  list.contains(reserved_generated_keys(), name)
}

pub fn sensitive_key(name: String) -> Bool {
  let segments =
    name
    |> string.uppercase
    |> string.split(on: "_")
    |> list.filter(fn(segment) { segment != "" })
  list.any(segments, sensitive_segment) || sensitive_final_pair(segments)
}

pub fn canonicalize(
  entries: List(#(String, String)),
) -> List(#(String, String)) {
  entries
  |> sort_entries
  |> unique_entries([], [])
}

pub fn merge(
  profile_env: List(#(String, String)),
  generated: List(#(String, String)),
) -> List(#(String, String)) {
  let generated_keys = env_keys(generated)
  let profile_env =
    profile_env
    |> canonicalize
    |> list.filter(fn(entry) {
      let #(key, _) = entry
      !list.contains(generated_keys, key)
    })
  list.append(profile_env, generated)
}

pub fn values_for_redaction(
  profile_env: List(#(String, String)),
) -> List(String) {
  profile_env
  |> canonicalize
  |> list.filter_map(fn(entry) {
    let #(key, value) = entry
    case value != "" && string.length(value) >= 8 && sensitive_key(key) {
      True -> Ok(value)
      False -> Error(Nil)
    }
  })
  |> list.sort(by: string.compare)
  |> unique_strings([])
}

pub fn fingerprint_entries(
  profile_env: List(#(String, String)),
) -> List(#(String, String)) {
  profile_env
  |> canonicalize
  |> list.map(fn(entry) {
    let #(key, value) = entry
    #(key, hash.sha256_hex(value))
  })
}

fn reserved_generated_keys() -> List(String) {
  [
    "SCHERZO_CONFIG_DIR",
    "SCHERZO_REPO_ROOT",
    "SCHERZO_WORKSPACE_DRIVER",
    "SCHERZO_WORKSPACE_CAPABILITIES",
    "SCHERZO_RUN_KIND",
    "SCHERZO_WORKFLOW_ID",
    "SCHERZO_WORKFLOW_BUNDLE_DIR",
    "SCHERZO_RUN_ID",
    "SCHERZO_RUN_ROOT",
    "SCHERZO_RUN_ARTIFACT_DIR",
    "SCHERZO_ISSUE_ID",
    "SCHERZO_ISSUE_IDENTIFIER",
    "SCHERZO_STEP_ID",
    "SCHERZO_ATTEMPT_INDEX",
    "SCHERZO_ATTEMPT_KEY",
    "SCHERZO_HOOK_IDEMPOTENCY_KEY",
    "SCHERZO_WORKSPACE_ROOT",
    "SCHERZO_WORKSPACE_PROFILE",
    "SCHERZO_WORKSPACE_NAME",
    "SCHERZO_WORKSPACE_PATH",
    "SCHERZO_SOURCE_WORKSPACE_NAME",
    "SCHERZO_SOURCE_WORKSPACE_PATH",
    "SCHERZO_SCHEDULED_JOB_ID",
    "SCHERZO_SCHEDULE_DUE_AT",
    "SCHERZO_SCHEDULE_STARTED_AT",
    "SCHERZO_RUN_ATTEMPT",
  ]
}

fn sort_entries(entries: List(#(String, String))) -> List(#(String, String)) {
  list.sort(entries, by: fn(left, right) {
    let #(left_key, _) = left
    let #(right_key, _) = right
    string.compare(left_key, right_key)
  })
}

fn unique_entries(
  entries: List(#(String, String)),
  seen: List(String),
  acc: List(#(String, String)),
) -> List(#(String, String)) {
  case entries {
    [] -> list.reverse(acc)
    [#(key, _) as entry, ..rest] ->
      case list.contains(seen, key) {
        True -> unique_entries(rest, seen, acc)
        False -> unique_entries(rest, [key, ..seen], [entry, ..acc])
      }
  }
}

fn env_keys(env: List(#(String, String))) -> List(String) {
  case env {
    [] -> []
    [#(key, _), ..rest] -> [key, ..env_keys(rest)]
  }
}

fn unique_strings(values: List(String), acc: List(String)) -> List(String) {
  case values {
    [] -> list.reverse(acc)
    [value, ..rest] ->
      case list.contains(acc, value) {
        True -> unique_strings(rest, acc)
        False -> unique_strings(rest, [value, ..acc])
      }
  }
}

fn sensitive_segment(segment: String) -> Bool {
  list.contains(
    ["SECRET", "TOKEN", "PASSWORD", "PASS", "CREDENTIAL", "CREDENTIALS", "AUTH"],
    segment,
  )
}

fn sensitive_final_pair(segments: List(String)) -> Bool {
  case last_two_segments(segments) {
    Ok(#("API", "KEY")) -> True
    Ok(#("ACCESS", "KEY")) -> True
    Ok(#("PRIVATE", "KEY")) -> True
    Ok(#("SESSION", "KEY")) -> True
    _ -> False
  }
}

fn last_two_segments(segments: List(String)) -> Result(#(String, String), Nil) {
  case segments {
    [left, right] -> Ok(#(left, right))
    [_, ..rest] -> last_two_segments(rest)
    _ -> Error(Nil)
  }
}

fn is_key_tail(ch: String) -> Bool {
  is_alpha(ch) || is_digit(ch) || ch == "_"
}

fn is_alpha(ch: String) -> Bool {
  is_between(ch, "A", "Z") || is_between(ch, "a", "z")
}

fn is_digit(ch: String) -> Bool {
  is_between(ch, "0", "9")
}

fn is_between(value: String, low: String, high: String) -> Bool {
  string.compare(value, low) != Lt && string.compare(value, high) != Gt
}

fn all(values: List(a), predicate: fn(a) -> Bool) -> Bool {
  case values {
    [] -> True
    [value, ..rest] -> predicate(value) && all(rest, predicate)
  }
}
