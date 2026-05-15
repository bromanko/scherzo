import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import yay

pub type Env =
  fn(String) -> Option(String)

pub fn default() -> config_types.TrackerConfig {
  config_types.TrackerConfig(
    kind: tracker_kind.LinearTracker,
    endpoint: "https://api.linear.app/graphql",
    api_key: None,
    project_slug: None,
    active_states: issue_state.list_from_strings(["Todo", "In Progress"]),
    dispatch_states: issue_state.list_from_strings(["Todo"]),
    terminal_states: issue_state.list_from_strings([
      "Closed",
      "Cancelled",
      "Canceled",
      "Duplicate",
      "Done",
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
  let linear_node = get_map(tracker_node, "linear")
  let credentials_node = get_map(tracker_node, "credentials")
  let kind =
    get_required_string(
      tracker_node,
      "kind",
      error.UnsupportedTrackerKind("missing"),
    )
  use kind <- result.try(kind)
  let normalized_kind = kind |> string.trim |> string.lowercase
  case tracker_kind.from_string(normalized_kind) {
    Ok(kind) -> {
      let endpoint =
        get_string(linear_node, "endpoint")
        |> option.lazy_or(fn() { get_string(tracker_node, "endpoint") })
        |> string_default("https://api.linear.app/graphql")
      use endpoint <- result.try(validate_https_endpoint(endpoint))
      let active_state_strings =
        get_string_list(tracker_node, "active_states")
        |> list_default(["Todo", "In Progress"])
      let active_states = issue_state.list_from_strings(active_state_strings)
      use dispatch_state_strings <- result.try(get_required_string_list_strict(
        tracker_node,
        "dispatch_states",
        "tracker.dispatch_states",
      ))
      use dispatch_states <- result.try(resolve_dispatch_states(
        active_states,
        dispatch_state_strings,
      ))
      let terminal_states =
        get_string_list(tracker_node, "terminal_states")
        |> list_default(["Closed", "Cancelled", "Canceled", "Duplicate", "Done"])
      let api_key = resolve_tracker_api_key(tracker_node, credentials_node, env)
      let project_slug =
        get_string(linear_node, "project_slug")
        |> option.lazy_or(fn() { get_string(tracker_node, "project_slug") })
        |> resolve_optional_env(env)
      use project_slug <- result.try(required_option(
        project_slug,
        error.MissingTrackerProjectSlug,
      ))
      use api_key <- result.try(required_option(
        api_key,
        error.MissingTrackerApiKey,
      ))
      Ok(#(
        config_types.TrackerConfig(
          kind: kind,
          endpoint: endpoint,
          api_key: Some(api_key),
          project_slug: Some(project_slug),
          active_states: active_states,
          dispatch_states: dispatch_states,
          terminal_states: issue_state.list_from_strings(terminal_states),
        ),
        tracker_alias_warnings(tracker_node, linear_node, credentials_node),
      ))
    }
    Error(_) -> Error(error.UnsupportedTrackerKind(normalized_kind))
  }
}

fn resolve_tracker_api_key(
  tracker_node: yay.Node,
  credentials_node: yay.Node,
  env: Env,
) -> Option(String) {
  case get_string(credentials_node, "api_key_env") {
    Some(name) -> resolve_env_name(name, env)
    None ->
      resolve_optional_env(get_string(tracker_node, "api_key"), env)
      |> option.lazy_or(fn() { env("LINEAR_API_KEY") })
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
    node_has_key(linear_node, "project_slug"),
    "tracker.project_slug",
    "tracker.linear.project_slug",
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
        "tracker.dispatch_states must contain at least one state",
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
            "tracker.dispatch_states must be a subset of tracker.active_states; invalid dispatch state "
            <> issue_state.to_string(candidate)
            <> ". Remove it from dispatch_states or add it to active_states only if it is truly lifecycle-active.",
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

fn get_required_string(
  node: yay.Node,
  key: String,
  err: error.ConfigError,
) -> Result(String, error.ConfigError) {
  case get_string(node, key) {
    Some(value) -> Ok(value)
    None -> Error(err)
  }
}

fn get_string(node: yay.Node, key: String) -> Option(String) {
  case get_node(node, key) {
    Some(yay.NodeStr(value)) -> Some(value)
    _ -> None
  }
}

fn get_string_list(node: yay.Node, key: String) -> Option(List(String)) {
  case get_node(node, key) {
    Some(yay.NodeSeq(values)) -> Some(string_values(values, []))
    _ -> None
  }
}

fn string_values(values: List(yay.Node), acc: List(String)) -> List(String) {
  case values {
    [] -> list.reverse(acc)
    [yay.NodeStr(value), ..rest] -> string_values(rest, [value, ..acc])
    [_, ..rest] -> string_values(rest, acc)
  }
}

fn get_required_string_list_strict(
  node: yay.Node,
  key: String,
  path: String,
) -> Result(List(String), error.ConfigError) {
  case get_node(node, key) {
    None ->
      Error(error.InvalidConfig(
        path <> " is required; add dispatch_states: [Todo] under tracker",
      ))
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

fn list_default(value: Option(List(a)), default: List(a)) -> List(a) {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn string_default(value: Option(String), default: String) -> String {
  case value {
    Some(value) -> value
    None -> default
  }
}
