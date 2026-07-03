import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/duration

pub const default_timeout_ms = 5000

pub const default_wait_timeout_ms = 120_000

pub type Settings {
  Settings(timeout_ms: Int, wait_timeout_ms: Int, wait: Bool)
}

pub type ResolutionError {
  InvalidDuration(message: String)
}

type StoredSettings {
  Undefined
  Stored(settings: Settings)
}

const timeout_env = "SCHERZO_CTL_TIMEOUT"

const wait_timeout_env = "SCHERZO_CTL_WAIT_TIMEOUT"

const settings_key = "scherzoctl-timeout-settings"

pub fn default_settings() -> Settings {
  Settings(
    timeout_ms: default_timeout_ms,
    wait_timeout_ms: default_wait_timeout_ms,
    wait: False,
  )
}

pub fn resolve(
  args: List(String),
  env: fn(String) -> Option(String),
) -> Result(Settings, ResolutionError) {
  use timeout_ms <- result.try(resolve_timeout_ms(args, env))
  use wait_timeout_ms <- result.try(resolve_wait_timeout_ms(args, env))
  Ok(Settings(
    timeout_ms: timeout_ms,
    wait_timeout_ms: wait_timeout_ms,
    wait: has_flag(args, "--wait"),
  ))
}

fn resolve_timeout_ms(
  args: List(String),
  env: fn(String) -> Option(String),
) -> Result(Int, ResolutionError) {
  case cli_option_value(args, "--timeout") {
    Some(value) -> parse_positive_ms(value, "--timeout")
    None ->
      case env(timeout_env) {
        Some(value) -> parse_positive_ms(value, timeout_env)
        None -> Ok(default_timeout_ms)
      }
  }
}

fn resolve_wait_timeout_ms(
  args: List(String),
  env: fn(String) -> Option(String),
) -> Result(Int, ResolutionError) {
  case cli_option_value(args, "--timeout") {
    Some(value) -> parse_positive_ms(value, "--timeout")
    None ->
      case env(wait_timeout_env) {
        Some(value) -> parse_positive_ms(value, wait_timeout_env)
        None ->
          case env(timeout_env) {
            Some(value) -> parse_positive_ms(value, timeout_env)
            None -> Ok(default_wait_timeout_ms)
          }
      }
  }
}

pub fn timeout_option_validator(value: String) -> Result(String, String) {
  case parse_positive_ms(value, "--timeout") {
    Ok(_) -> Ok(value)
    Error(InvalidDuration(message)) -> Error(message)
  }
}

fn parse_positive_ms(
  value: String,
  field: String,
) -> Result(Int, ResolutionError) {
  duration.parse_positive_ms(value, field)
  |> result.map_error(fn(error) {
    InvalidDuration(duration.error_message(error))
  })
}

fn cli_option_value(args: List(String), name: String) -> Option(String) {
  case args {
    [] -> None
    [first, second, ..] if first == name -> Some(second)
    [first, ..rest] ->
      case string.starts_with(first, name <> "=") {
        True -> Some(string.drop_start(first, string.length(name) + 1))
        False -> cli_option_value(rest, name)
      }
  }
}

fn has_flag(args: List(String), name: String) -> Bool {
  case args {
    [] -> False
    [first, ..rest] ->
      case first == name {
        True -> True
        False -> has_flag(rest, name)
      }
  }
}

pub fn put_current(settings: Settings) -> Nil {
  let _ = put_settings(settings_key, Stored(settings))
  Nil
}

pub fn get_current() -> Settings {
  case get_settings(settings_key) {
    Stored(settings) -> settings
    Undefined -> default_settings()
  }
}

pub fn clear_current() -> Nil {
  let _ = put_settings(settings_key, Undefined)
  Nil
}

pub fn current_timeout_ms() -> Int {
  get_current().timeout_ms
}

pub fn current_wait_timeout_ms() -> Int {
  get_current().wait_timeout_ms
}

pub fn current_wait() -> Bool {
  get_current().wait
}

@external(erlang, "erlang", "put")
fn put_settings(key: String, value: StoredSettings) -> b

@external(erlang, "erlang", "get")
fn get_settings(key: String) -> StoredSettings
