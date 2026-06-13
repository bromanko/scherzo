import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import scherzo/control/defaults as control_defaults
import scherzo/duration
import scherzo/error
import yay

type DurationFieldSource {
  DurationField(node: yay.Node, key: String, path: String)
  LegacyMillisecondsField(node: yay.Node, key: String, path: String)
}

pub const default_control_command_timeout_ms = control_defaults.default_command_timeout_ms

pub fn polling_interval_ms(root: yay.Node) -> Result(Int, error.ConfigError) {
  let tracker = get_map(root, "tracker")
  let tracker_polling = get_map(tracker, "polling")
  let polling = get_map(root, "polling")
  get_duration_ms_from_sources(
    [
      DurationField(tracker_polling, "every", "tracker.polling.every"),
      DurationField(polling, "interval", "polling.interval"),
      LegacyMillisecondsField(polling, "interval_ms", "polling.interval_ms"),
    ],
    30_000,
    False,
  )
}

pub fn hooks_timeout_ms(root: yay.Node) -> Result(Int, error.ConfigError) {
  let hooks = get_map(root, "hooks")
  get_duration_ms_from_sources(
    [
      DurationField(hooks, "timeout", "hooks.timeout"),
      LegacyMillisecondsField(hooks, "timeout_ms", "hooks.timeout_ms"),
    ],
    60_000,
    False,
  )
}

pub fn control_command_timeout_ms(
  root: yay.Node,
  default: Int,
) -> Result(Int, error.ConfigError) {
  let control = get_map(root, "control")
  get_duration_ms_from_sources(
    [DurationField(control, "command_timeout", "control.command_timeout")],
    default,
    False,
  )
}

pub fn pi_turn_timeout_ms(root: yay.Node) -> Result(Int, error.ConfigError) {
  let pi = get_map(root, "pi")
  let runtime = runtime_config(root)
  get_duration_ms_from_sources(
    [
      DurationField(runtime, "turn_timeout", "agents.runtime.turn_timeout"),
      DurationField(pi, "turn_timeout", "pi.turn_timeout"),
      LegacyMillisecondsField(pi, "turn_timeout_ms", "pi.turn_timeout_ms"),
    ],
    3_600_000,
    False,
  )
}

pub fn pi_read_timeout_ms(root: yay.Node) -> Result(Int, error.ConfigError) {
  let pi = get_map(root, "pi")
  let runtime = runtime_config(root)
  get_duration_ms_from_sources(
    [
      DurationField(runtime, "read_timeout", "agents.runtime.read_timeout"),
      DurationField(pi, "read_timeout", "pi.read_timeout"),
      LegacyMillisecondsField(pi, "read_timeout_ms", "pi.read_timeout_ms"),
    ],
    5000,
    False,
  )
}

pub fn pi_stall_timeout_ms(root: yay.Node) -> Result(Int, error.ConfigError) {
  let pi = get_map(root, "pi")
  let runtime = runtime_config(root)
  get_duration_ms_from_sources(
    [
      DurationField(runtime, "stall_timeout", "agents.runtime.stall_timeout"),
      DurationField(pi, "stall_timeout", "pi.stall_timeout"),
      LegacyMillisecondsField(pi, "stall_timeout_ms", "pi.stall_timeout_ms"),
    ],
    300_000,
    True,
  )
}

pub fn pi_ui_request_timeout_ms(
  root: yay.Node,
) -> Result(Int, error.ConfigError) {
  let pi = get_map(root, "pi")
  let runtime = runtime_config(root)
  get_duration_ms_from_sources(
    [
      DurationField(
        runtime,
        "ui_request_timeout",
        "agents.runtime.ui_request_timeout",
      ),
      DurationField(pi, "ui_request_timeout", "pi.ui_request_timeout"),
      LegacyMillisecondsField(
        pi,
        "ui_request_timeout_ms",
        "pi.ui_request_timeout_ms",
      ),
    ],
    300_000,
    False,
  )
}

pub fn workspace_driver_timeout_ms(
  driver: yay.Node,
  path: String,
) -> Result(Int, error.ConfigError) {
  get_duration_ms_from_sources(
    [
      DurationField(driver, "timeout", path <> ".timeout"),
      LegacyMillisecondsField(driver, "timeout_ms", path <> ".timeout_ms"),
    ],
    60_000,
    False,
  )
}

pub fn scheduled_every_ms(
  value: String,
  path: String,
) -> Result(Int, error.ConfigError) {
  duration.parse_positive_ms(value, "scheduled job every")
  |> result.map_error(fn(duration_error) {
    error.InvalidConfig(path <> ": " <> duration.error_message(duration_error))
  })
}

fn runtime_config(root: yay.Node) -> yay.Node {
  root
  |> get_map("agents")
  |> get_map("runtime")
}

fn get_duration_ms_from_sources(
  sources: List(DurationFieldSource),
  default: Int,
  allow_zero: Bool,
) -> Result(Int, error.ConfigError) {
  case sources {
    [] -> Ok(default)
    [DurationField(node, key, path), ..rest] -> {
      use value <- result.try(get_duration_ms(node, key, path, allow_zero))
      case value {
        Some(value) -> Ok(value)
        None -> get_duration_ms_from_sources(rest, default, allow_zero)
      }
    }
    [LegacyMillisecondsField(node, key, path), ..rest] -> {
      use value <- result.try(get_legacy_duration_ms(
        node,
        key,
        path,
        allow_zero,
      ))
      case value {
        Some(value) -> Ok(value)
        None -> get_duration_ms_from_sources(rest, default, allow_zero)
      }
    }
  }
}

fn get_duration_ms(
  node: yay.Node,
  key: String,
  path: String,
  allow_zero: Bool,
) -> Result(Option(Int), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeStr(value)) ->
      parse_duration_string(value, path, allow_zero) |> result.map(Some)
    Some(_) ->
      Error(error.InvalidConfig(
        path <> " must be a duration string with unit ms, s, m, or h",
      ))
  }
}

fn parse_duration_string(
  value: String,
  path: String,
  allow_zero: Bool,
) -> Result(Int, error.ConfigError) {
  let parsed = case allow_zero {
    True -> duration.parse_non_negative_ms(value, path)
    False -> duration.parse_positive_ms(value, path)
  }
  parsed
  |> result.map_error(fn(error) {
    error.InvalidConfig(duration.error_message(error))
  })
}

fn get_legacy_duration_ms(
  node: yay.Node,
  key: String,
  path: String,
  allow_zero: Bool,
) -> Result(Option(Int), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeInt(value)) ->
      validate_legacy_duration_ms(value, path, allow_zero) |> result.map(Some)
    Some(_) -> Error(error.InvalidConfig(path <> " must be an integer"))
  }
}

fn validate_legacy_duration_ms(
  value: Int,
  path: String,
  allow_zero: Bool,
) -> Result(Int, error.ConfigError) {
  case allow_zero {
    True ->
      case value < 0 {
        True -> Error(error.InvalidConfig(path <> " must be zero or positive"))
        False -> Ok(value)
      }
    False ->
      case value <= 0 {
        True -> Error(error.InvalidConfig(path <> " must be positive"))
        False -> Ok(value)
      }
  }
}

fn get_map(node: yay.Node, key: String) -> yay.Node {
  case get_node(node, key) {
    Some(value) -> value
    None -> yay.NodeMap([])
  }
}

fn get_node(node: yay.Node, key: String) -> Option(yay.Node) {
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(value) -> Some(value)
        Error(Nil) -> None
      }
    _ -> None
  }
}
