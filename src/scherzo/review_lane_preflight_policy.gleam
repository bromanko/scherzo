import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/control/file as control_file

pub type PreflightMode {
  Off
  OfflineRequired
  RequiredLive
}

pub type Policy {
  Policy(
    mode: PreflightMode,
    cache_ttl_seconds: Int,
    park_on_failure: Bool,
    strict_live_model_checks: Bool,
  )
}

pub fn default() -> Policy {
  Policy(
    mode: OfflineRequired,
    cache_ttl_seconds: 86_400,
    park_on_failure: True,
    strict_live_model_checks: False,
  )
}

pub fn from_env() -> Policy {
  let base = default()
  Policy(
    mode: env_mode("SCHERZO_REVIEW_LANE_PREFLIGHT_MODE")
      |> option.unwrap(base.mode),
    cache_ttl_seconds: env_int(
      "SCHERZO_REVIEW_LANE_PREFLIGHT_CACHE_TTL_SECONDS",
    )
      |> option.unwrap(base.cache_ttl_seconds),
    park_on_failure: env_bool("SCHERZO_REVIEW_LANE_PREFLIGHT_PARK_ON_FAILURE")
      |> option.unwrap(base.park_on_failure),
    strict_live_model_checks: env_bool(
      "SCHERZO_REVIEW_LANE_PREFLIGHT_STRICT_LIVE_MODEL_CHECKS",
    )
      |> option.unwrap(base.strict_live_model_checks),
  )
}

pub fn mode_to_string(mode: PreflightMode) -> String {
  case mode {
    Off -> "off"
    OfflineRequired -> "offline"
    RequiredLive -> "required-live"
  }
}

pub fn mode_from_string(value: String) -> Result(PreflightMode, Nil) {
  case value |> string.trim |> string.lowercase {
    "off" -> Ok(Off)
    "offline" -> Ok(OfflineRequired)
    "required-live" -> Ok(RequiredLive)
    _ -> Error(Nil)
  }
}

fn env_mode(name: String) -> Option(PreflightMode) {
  use value <- option.then(control_file.get_env(name))
  mode_from_string(value) |> option.from_result
}

fn env_int(name: String) -> Option(Int) {
  use value <- option.then(control_file.get_env(name))
  value
  |> string.trim
  |> int.parse
  |> option.from_result
}

fn env_bool(name: String) -> Option(Bool) {
  use value <- option.then(control_file.get_env(name))
  case value |> string.trim |> string.lowercase {
    "1" | "true" | "yes" | "on" -> Some(True)
    "0" | "false" | "no" | "off" -> Some(False)
    _ -> None
  }
}
