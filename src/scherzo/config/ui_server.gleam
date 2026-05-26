import gleam/option
import scherzo/config
import scherzo/config/types as config_types
import scherzo/log

pub fn debug_summary(config_value: config_types.EffectiveConfig) -> String {
  log.format(
    "debug",
    "ui_server_config",
    [
      #("enabled", bool_string(config_value.ui_server.enabled)),
      #("endpoint", option.unwrap(config_value.ui_server.endpoint, "")),
      #(
        "enrollment_token_env",
        option.unwrap(config_value.ui_server.enrollment_token_env, ""),
      ),
      #(
        "enrollment_token",
        option.unwrap(config_value.ui_server.enrollment_token, ""),
      ),
    ],
    config.resolved_secrets(config_value),
  )
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}
