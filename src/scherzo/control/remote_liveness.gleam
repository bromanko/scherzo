import gleam/dict
import gleam/int
import gleam/list
import gleam/order.{type Order}
import gleam/string

pub opaque type Registry {
  Registry(
    stale_after_ms: Int,
    offline_after_ms: Int,
    entries: dict.Dict(String, Entry),
  )
}

pub type Status {
  Online
  Stale
  Offline
}

pub type View {
  View(
    daemon_id: String,
    boot_id: String,
    status: Status,
    last_seen_at_ms: Int,
    observed_at_ms: Int,
  )
}

pub type RegistryError {
  InvalidDaemonId(String)
  InvalidBootId(String)
  InvalidThresholds(stale_after_ms: Int, offline_after_ms: Int)
  UnknownDaemon(String)
  StaleBoot(expected_boot_id: String, actual_boot_id: String)
}

type Entry {
  Entry(boot_id: String, last_seen_at_ms: Int)
}

pub fn new(
  stale_after_ms stale_after_ms: Int,
  offline_after_ms offline_after_ms: Int,
) -> Result(Registry, RegistryError) {
  case stale_after_ms >= 0 && offline_after_ms > stale_after_ms {
    True -> Ok(Registry(stale_after_ms, offline_after_ms, dict.new()))
    False -> Error(InvalidThresholds(stale_after_ms, offline_after_ms))
  }
}

pub fn register_hello(
  registry: Registry,
  daemon_id: String,
  boot_id: String,
  now_ms: Int,
) -> Result(Registry, RegistryError) {
  use _ <- result_try(validate_daemon_id(daemon_id))
  use _ <- result_try(validate_boot_id(boot_id))
  Ok(Registry(
    stale_after_ms: registry.stale_after_ms,
    offline_after_ms: registry.offline_after_ms,
    entries: dict.insert(registry.entries, daemon_id, Entry(boot_id, now_ms)),
  ))
}

pub fn heartbeat(
  registry: Registry,
  daemon_id: String,
  boot_id: String,
  now_ms: Int,
) -> Result(Registry, RegistryError) {
  use _ <- result_try(validate_daemon_id(daemon_id))
  use _ <- result_try(validate_boot_id(boot_id))
  case dict.get(registry.entries, daemon_id) {
    Error(Nil) -> Error(UnknownDaemon(daemon_id))
    Ok(Entry(active_boot_id, _)) if active_boot_id != boot_id ->
      Error(StaleBoot(active_boot_id, boot_id))
    Ok(_) ->
      Ok(Registry(
        stale_after_ms: registry.stale_after_ms,
        offline_after_ms: registry.offline_after_ms,
        entries: dict.insert(
          registry.entries,
          daemon_id,
          Entry(boot_id, now_ms),
        ),
      ))
  }
}

pub fn view(
  registry: Registry,
  daemon_id: String,
  now_ms: Int,
) -> Result(View, RegistryError) {
  use _ <- result_try(validate_daemon_id(daemon_id))
  case dict.get(registry.entries, daemon_id) {
    Error(Nil) -> Error(UnknownDaemon(daemon_id))
    Ok(entry) -> Ok(view_from_entry(registry, daemon_id, entry, now_ms))
  }
}

pub fn snapshot(registry: Registry, now_ms: Int) -> List(View) {
  registry.entries
  |> dict.to_list
  |> list.sort(by: compare_entry_keys)
  |> list.map(fn(item) {
    let #(daemon_id, entry) = item
    view_from_entry(registry, daemon_id, entry, now_ms)
  })
}

fn compare_entry_keys(
  left: #(String, Entry),
  right: #(String, Entry),
) -> Order {
  let #(left_key, _) = left
  let #(right_key, _) = right
  string.compare(left_key, right_key)
}

fn view_from_entry(
  registry: Registry,
  daemon_id: String,
  entry: Entry,
  now_ms: Int,
) -> View {
  let Entry(boot_id, last_seen_at_ms) = entry
  let age_ms = now_ms - last_seen_at_ms
  let status = case age_ms >= registry.offline_after_ms {
    True -> Offline
    False ->
      case age_ms >= registry.stale_after_ms {
        True -> Stale
        False -> Online
      }
  }
  View(
    daemon_id: daemon_id,
    boot_id: boot_id,
    status: status,
    last_seen_at_ms: last_seen_at_ms,
    observed_at_ms: now_ms,
  )
}

fn validate_daemon_id(value: String) -> Result(Nil, RegistryError) {
  case valid_prefixed_hex_id(value, "daemon_") {
    True -> Ok(Nil)
    False -> Error(InvalidDaemonId(value))
  }
}

fn validate_boot_id(value: String) -> Result(Nil, RegistryError) {
  case valid_prefixed_hex_id(value, "boot_") {
    True -> Ok(Nil)
    False -> Error(InvalidBootId(value))
  }
}

fn valid_prefixed_hex_id(value: String, prefix: String) -> Bool {
  string.starts_with(value, prefix)
  && string.length(value) == string.length(prefix) + 32
  && value
  |> string.drop_start(string.length(prefix))
  |> is_lower_hex_string
}

fn is_lower_hex_string(value: String) -> Bool {
  case string.to_graphemes(value) {
    [] -> False
    chars -> list.all(chars, is_lower_hex_char)
  }
}

fn is_lower_hex_char(char: String) -> Bool {
  case char {
    "0"
    | "1"
    | "2"
    | "3"
    | "4"
    | "5"
    | "6"
    | "7"
    | "8"
    | "9"
    | "a"
    | "b"
    | "c"
    | "d"
    | "e"
    | "f" -> True
    _ -> False
  }
}

fn result_try(
  result: Result(a, e),
  next: fn(a) -> Result(b, e),
) -> Result(b, e) {
  case result {
    Ok(value) -> next(value)
    Error(error) -> Error(error)
  }
}

pub fn status_to_string(status: Status) -> String {
  case status {
    Online -> "online"
    Stale -> "stale"
    Offline -> "offline"
  }
}

pub fn error_message(error: RegistryError) -> String {
  case error {
    InvalidDaemonId(value) -> "invalid daemon_id: " <> value
    InvalidBootId(value) -> "invalid boot_id: " <> value
    InvalidThresholds(stale_after_ms, offline_after_ms) ->
      "invalid thresholds stale_after_ms="
      <> int.to_string(stale_after_ms)
      <> " offline_after_ms="
      <> int.to_string(offline_after_ms)
    UnknownDaemon(daemon_id) -> "unknown daemon_id: " <> daemon_id
    StaleBoot(expected_boot_id, actual_boot_id) ->
      "stale boot heartbeat expected "
      <> expected_boot_id
      <> " got "
      <> actual_boot_id
  }
}
