import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/root_schema
import scherzo/config/types as config_types
import scherzo/error
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import yay

pub type Env =
  fn(String) -> Option(String)

pub fn reject_root_removed_keys(
  root: yay.Node,
) -> Result(Nil, error.ConfigError) {
  root_schema.reject_removed_keys(root)
}

pub fn resolve_root_routing(
  root: yay.Node,
  config_path: String,
) -> Result(config_types.RoutingConfig, error.ConfigError) {
  root_schema.resolve_routing(root, config_path)
}

pub fn resolve_root_linear_contract_fields(
  root: yay.Node,
) -> Result(root_schema.SimplifiedLinearContractFields, error.ConfigError) {
  root_schema.resolve_linear_contract_fields(root)
}

pub fn apply_root_linear_contract_fields(
  contract: config_types.LinearContractConfig,
  fields: root_schema.SimplifiedLinearContractFields,
) -> config_types.LinearContractConfig {
  root_schema.apply_linear_contract_fields(contract, fields)
}

pub fn resolve_root_orchestrator_linear_contract(
  root: yay.Node,
  effective: config_types.EffectiveConfig,
  routing: config_types.RoutingConfig,
  scheduled_jobs: List(config_types.ScheduledJobConfig),
) -> Result(config_types.LinearContractConfig, error.ConfigError) {
  root_schema.resolve_orchestrator_linear_contract(
    root,
    effective,
    routing,
    scheduled_jobs,
  )
}

pub fn default() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    kind: tracker_kind.LinearTracker,
    endpoint: "https://api.linear.app/graphql",
    api_key: None,
    project_slug: None,
    active_states: issue_state.list_from_strings(["Todo", "In Progress"]),
    dispatch_states: issue_state.list_from_strings(["Todo"]),
    terminal_states: issue_state.list_from_strings([
      "Done",
      "Canceled",
      "Cancelled",
      "Duplicate",
    ]),
  )
}

pub fn resolve(
  root: yay.Node,
  env: Env,
) -> Result(
  #(config_types.TrackerConfig, List(config_types.ConfigWarning)),
  error.ConfigError,
) {
  let tracker_node = get_map(root, "tracker")
  use _ <- result.try(reject_removed_tracker_state_keys(tracker_node))
  use states_node <- result.try(get_map_strict_or_empty(
    tracker_node,
    "states",
    "tracker.states",
  ))
  let linear_node = get_map(tracker_node, "linear")
  let credentials_node = get_map(tracker_node, "credentials")
  use kind <- result.try(resolve_tracker_kind(tracker_node))
  let endpoint =
    get_string(linear_node, "endpoint")
    |> option.lazy_or(fn() { get_string(tracker_node, "endpoint") })
    |> string_default("https://api.linear.app/graphql")
  use endpoint <- result.try(validate_https_endpoint(endpoint))
  use active_state_strings <- result.try(
    get_string_list_strict(states_node, "active", "tracker.states.active", [
      "Todo",
      "In Progress",
    ]),
  )
  let active_states = issue_state.list_from_strings(active_state_strings)
  use ready_state_strings <- result.try(
    get_string_list_strict(states_node, "ready", "tracker.states.ready", [
      "Todo",
    ]),
  )
  use dispatch_states <- result.try(resolve_dispatch_states(
    active_states,
    ready_state_strings,
  ))
  use terminal_state_strings <- result.try(
    get_string_list_strict(states_node, "terminal", "tracker.states.terminal", [
      "Done",
      "Canceled",
      "Cancelled",
      "Duplicate",
    ]),
  )
  let api_key =
    resolve_tracker_api_key(tracker_node, linear_node, credentials_node, env)
  let project_slug =
    get_string(linear_node, "project")
    |> option.lazy_or(fn() { get_string(linear_node, "project_slug") })
    |> option.lazy_or(fn() { get_string(tracker_node, "project_slug") })
    |> resolve_optional_env(env)
  use project_slug <- result.try(required_option(
    project_slug,
    error.MissingTrackerProjectSlug,
  ))
  use api_key <- result.try(required_option(api_key, error.MissingTrackerApiKey))
  Ok(#(
    config_types.TrackerConfig(
      kind: kind,
      endpoint: endpoint,
      api_key: Some(api_key),
      project_slug: Some(project_slug),
      active_states: active_states,
      dispatch_states: dispatch_states,
      terminal_states: issue_state.list_from_strings(terminal_state_strings),
    ),
    tracker_alias_warnings(tracker_node, linear_node, credentials_node),
  ))
}

fn resolve_tracker_kind(
  tracker_node: yay.Node,
) -> Result(tracker_kind.TrackerKind, error.ConfigError) {
  let normalized_kind =
    get_string(tracker_node, "kind")
    |> string_default("linear")
    |> string.trim
    |> string.lowercase
  case tracker_kind.from_string(normalized_kind) {
    Ok(kind) -> Ok(kind)
    Error(_) -> Error(error.UnsupportedTrackerKind(normalized_kind))
  }
}

fn reject_removed_tracker_state_keys(
  tracker_node: yay.Node,
) -> Result(Nil, error.ConfigError) {
  case node_has_key(tracker_node, "dispatch_states") {
    True ->
      Error(error.InvalidConfig(
        "tracker.dispatch_states was removed. Use tracker.states.ready. See docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md.",
      ))
    False ->
      case node_has_key(tracker_node, "active_states") {
        True ->
          Error(error.InvalidConfig(
            "tracker.active_states was removed. Use tracker.states.active. See docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md.",
          ))
        False ->
          case node_has_key(tracker_node, "terminal_states") {
            True ->
              Error(error.InvalidConfig(
                "tracker.terminal_states was removed. Use tracker.states.terminal. See docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md.",
              ))
            False -> Ok(Nil)
          }
      }
  }
}

fn resolve_tracker_api_key(
  tracker_node: yay.Node,
  linear_node: yay.Node,
  credentials_node: yay.Node,
  env: Env,
) -> Option(String) {
  case get_string(linear_node, "api_key_env") {
    Some(name) -> resolve_env_name(name, env)
    None ->
      case get_string(credentials_node, "api_key_env") {
        Some(name) -> resolve_env_name(name, env)
        None ->
          resolve_optional_env(get_string(tracker_node, "api_key"), env)
          |> option.lazy_or(fn() { env("LINEAR_API_KEY") })
      }
  }
}

fn resolve_env_name(name: String, env: Env) -> Option(String) {
  let name = string.trim(name)
  let name = case string.starts_with(name, "$") {
    True -> string.drop_start(name, 1)
    False -> name
  }
  case name == "" {
    True -> None
    False -> env(name)
  }
}

fn tracker_alias_warnings(
  tracker_node: yay.Node,
  linear_node: yay.Node,
  credentials_node: yay.Node,
) -> List(config_types.ConfigWarning) {
  []
  |> append_legacy_tracker_warning(
    node_has_key(tracker_node, "api_key"),
    node_has_key(credentials_node, "api_key_env"),
    "tracker.api_key",
    "tracker.credentials.api_key_env",
  )
  |> append_legacy_tracker_warning(
    node_has_key(tracker_node, "endpoint"),
    node_has_key(linear_node, "endpoint"),
    "tracker.endpoint",
    "tracker.linear.endpoint",
  )
  |> append_legacy_tracker_warning(
    node_has_key(tracker_node, "project_slug"),
    node_has_key(linear_node, "project")
      || node_has_key(linear_node, "project_slug"),
    "tracker.project_slug",
    "tracker.linear.project",
  )
}

fn append_legacy_tracker_warning(
  warnings: List(config_types.ConfigWarning),
  legacy_present: Bool,
  preferred_present: Bool,
  path: String,
  replacement: String,
) -> List(config_types.ConfigWarning) {
  case legacy_present && preferred_present {
    True ->
      list.append(warnings, [
        config_types.ConfigWarning(
          event: "legacy_tracker_field_ignored",
          path: path,
          replacement: replacement,
        ),
      ])
    False -> warnings
  }
}

fn resolve_dispatch_states(
  active_states: List(issue_state.IssueState),
  raw_dispatch_states: List(String),
) -> Result(List(issue_state.IssueState), error.ConfigError) {
  case raw_dispatch_states {
    [] ->
      Error(error.InvalidConfig(
        "tracker.states.ready must contain at least one state",
      ))
    _ -> canonicalize_dispatch_states(active_states, raw_dispatch_states, [])
  }
}

fn canonicalize_dispatch_states(
  active_states: List(issue_state.IssueState),
  raw_dispatch_states: List(String),
  acc: List(issue_state.IssueState),
) -> Result(List(issue_state.IssueState), error.ConfigError) {
  case raw_dispatch_states {
    [] -> Ok(list.reverse(acc))
    [raw, ..rest] -> {
      let candidate = issue_state.from_string_unchecked(raw)
      case issue_state.canonicalize_against(active_states, candidate) {
        Ok(canonical) ->
          canonicalize_dispatch_states(active_states, rest, [canonical, ..acc])
        Error(_) ->
          Error(error.InvalidConfig(
            "tracker.states.ready must be a subset of tracker.states.active; invalid ready state "
            <> issue_state.to_string(candidate)
            <> ". Remove it from tracker.states.ready or add it to tracker.states.active only if it is truly lifecycle-active.",
          ))
      }
    }
  }
}

fn validate_https_endpoint(
  endpoint: String,
) -> Result(String, error.ConfigError) {
  let endpoint = string.trim(endpoint)
  case string.starts_with(string.lowercase(endpoint), "https://") {
    True -> Ok(endpoint)
    False -> Error(error.InvalidConfig("tracker.endpoint must use https://"))
  }
}

fn get_map(node: yay.Node, key: String) -> yay.Node {
  case get_node(node, key) {
    Some(value) -> value
    None -> yay.NodeMap([])
  }
}

fn get_map_strict_or_empty(
  node: yay.Node,
  key: String,
  path: String,
) -> Result(yay.Node, error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(yay.NodeMap([]))
    Some(yay.NodeMap(_) as value) -> Ok(value)
    Some(_) -> Error(error.InvalidConfig(path <> " must be a map"))
  }
}

fn get_node(node: yay.Node, key: String) -> Option(yay.Node) {
  case node {
    yay.NodeMap(pairs) -> get_pair(pairs, key)
    _ -> None
  }
}

fn get_pair(
  pairs: List(#(yay.Node, yay.Node)),
  key: String,
) -> Option(yay.Node) {
  case pairs {
    [] -> None
    [#(yay.NodeStr(candidate), value), ..rest] ->
      case candidate == key {
        True -> Some(value)
        False -> get_pair(rest, key)
      }
    [#(_, _), ..rest] -> get_pair(rest, key)
  }
}

fn node_has_key(node: yay.Node, key: String) -> Bool {
  case get_node(node, key) {
    Some(_) -> True
    None -> False
  }
}

fn get_string(node: yay.Node, key: String) -> Option(String) {
  case get_node(node, key) {
    Some(yay.NodeStr(value)) -> Some(value)
    _ -> None
  }
}

fn get_string_list_strict(
  node: yay.Node,
  key: String,
  path: String,
  default: List(String),
) -> Result(List(String), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(default)
    Some(yay.NodeSeq(values)) -> read_string_values(values, path, [])
    Some(_) -> Error(error.InvalidConfig(path <> " must be a string list"))
  }
}

fn read_string_values(
  values: List(yay.Node),
  path: String,
  acc: List(String),
) -> Result(List(String), error.ConfigError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [yay.NodeStr(value), ..rest] ->
      read_string_values(rest, path, [value, ..acc])
    [_, ..] -> Error(error.InvalidConfig(path <> " entries must be strings"))
  }
}

fn resolve_optional_env(value: Option(String), env: Env) -> Option(String) {
  case value {
    Some(value) ->
      case is_single_env_reference(value) {
        True -> env(string.drop_start(value, 1))
        False -> Some(value)
      }
    None -> None
  }
}

fn is_single_env_reference(value: String) -> Bool {
  string.starts_with(value, "$")
  && string.length(value) > 1
  && !string.contains(value, "/")
  && !string.contains(value, " ")
}

fn required_option(
  value: Option(a),
  err: error.ConfigError,
) -> Result(a, error.ConfigError) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(err)
  }
}

fn string_default(value: Option(String), default: String) -> String {
  case value {
    Some(value) -> value
    None -> default
  }
}
