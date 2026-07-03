import gleam/option.{None, Some}
import scherzo/ctl/timeout_settings

pub fn resolve_supports_duration_units_test() {
  let assert Ok(settings_500ms) =
    timeout_settings.resolve(["ping", "--timeout", "500ms"], fn(_) { None })
  assert settings_500ms.timeout_ms == 500
  assert settings_500ms.wait_timeout_ms == 500

  let assert Ok(settings_5s) =
    timeout_settings.resolve(["ping", "--timeout", "5s"], fn(_) { None })
  assert settings_5s.timeout_ms == 5000

  let assert Ok(settings_2m) =
    timeout_settings.resolve(
      ["query", "operation-status", "op-1", "--timeout", "2m"],
      fn(_) { None },
    )
  assert settings_2m.timeout_ms == 120_000
  assert settings_2m.wait_timeout_ms == 120_000
}

pub fn resolve_rejects_invalid_duration_test() {
  let assert Error(timeout_settings.InvalidDuration(message)) =
    timeout_settings.resolve(["ping", "--timeout", "soon"], fn(_) { None })
  assert message == "--timeout must use unit ms, s, m, or h"
}

pub fn resolve_uses_env_defaults_test() {
  let assert Ok(settings) =
    timeout_settings.resolve(["ping"], fn(name) {
      case name {
        "SCHERZO_CTL_TIMEOUT" -> Some("5s")
        "SCHERZO_CTL_WAIT_TIMEOUT" -> Some("2m")
        _ -> None
      }
    })
  assert settings.timeout_ms == 5000
  assert settings.wait_timeout_ms == 120_000
}

pub fn resolve_prefers_cli_timeout_over_env_test() {
  let assert Ok(settings) =
    timeout_settings.resolve(
      ["query", "operation-status", "op-1", "--wait", "--timeout", "500ms"],
      fn(name) {
        case name {
          "SCHERZO_CTL_TIMEOUT" -> Some("5s")
          "SCHERZO_CTL_WAIT_TIMEOUT" -> Some("2m")
          _ -> None
        }
      },
    )
  assert settings.wait
  assert settings.timeout_ms == 500
  assert settings.wait_timeout_ms == 500
}
