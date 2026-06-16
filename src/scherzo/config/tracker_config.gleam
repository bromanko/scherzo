import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/config/linear_task_scope
import scherzo/config/root_schema
import scherzo/config/types as config_types
import scherzo/config/workspace_driver_config
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

pub fn resolve_workspace_drivers(
  workspace: yay.Node,
) -> Result(config_types.WorkspaceHookProfiles, error.ConfigError) {
  workspace_driver_config.resolve(workspace)
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
    task_scope: None,
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
  use task_scope <- result.try(resolve_task_scope(
    tracker_node,
    linear_node,
    env,
  ))
  use api_key <- result.try(required_option(api_key, error.MissingTrackerApiKey))
  Ok(#(
    config_types.TrackerConfig(
      kind: kind,
      endpoint: endpoint,
      api_key: Some(api_key),
      project_slug: primary_project_slug(task_scope),
      task_scope: Some(task_scope),
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

fn resolve_task_scope(
  tracker_node: yay.Node,
  linear_node: yay.Node,
  env: Env,
) -> Result(config_types.LinearTaskScope, error.ConfigError) {
  case get_node(linear_node, "tasks_from") {
    Some(tasks_from) -> {
      use _ <- result.try(reject_tasks_from_legacy_conflicts(
        tracker_node,
        linear_node,
      ))
      parse_tasks_from(tasks_from, env)
    }
    None -> resolve_legacy_project_scope(tracker_node, linear_node, env)
  }
}

fn resolve_legacy_project_scope(
  tracker_node: yay.Node,
  linear_node: yay.Node,
  env: Env,
) -> Result(config_types.LinearTaskScope, error.ConfigError) {
  use #(path, raw_project_slug) <- result.try(required_option(
    legacy_project_value(tracker_node, linear_node),
    error.MissingTrackerProjectSlug,
  ))
  use project_slug <- result.try(required_option(
    resolve_optional_env(Some(raw_project_slug), env),
    error.InvalidConfig(path <> " must resolve to a string"),
  ))
  validate_project_slug(project_slug, path, fn(slug) {
    config_types.LinearTaskProject(slug)
  })
}

fn legacy_project_value(
  tracker_node: yay.Node,
  linear_node: yay.Node,
) -> Option(#(String, String)) {
  case get_string(linear_node, "project") {
    Some(project) -> Some(#("tracker.linear.project", project))
    None ->
      case get_string(linear_node, "project_slug") {
        Some(project_slug) ->
          Some(#("tracker.linear.project_slug", project_slug))
        None ->
          case get_string(tracker_node, "project_slug") {
            Some(project_slug) -> Some(#("tracker.project_slug", project_slug))
            None -> None
          }
      }
  }
}

fn reject_tasks_from_legacy_conflicts(
  tracker_node: yay.Node,
  linear_node: yay.Node,
) -> Result(Nil, error.ConfigError) {
  case node_has_key(linear_node, "project") {
    True -> Error(task_scope_conflict("tracker.linear.project"))
    False ->
      case node_has_key(linear_node, "project_slug") {
        True -> Error(task_scope_conflict("tracker.linear.project_slug"))
        False ->
          case node_has_key(tracker_node, "project_slug") {
            True -> Error(task_scope_conflict("tracker.project_slug"))
            False -> Ok(Nil)
          }
      }
  }
}

fn task_scope_conflict(path: String) -> error.ConfigError {
  error.InvalidConfig(
    "tracker.linear.tasks_from cannot be combined with "
    <> path
    <> ". Choose one task-scope configuration surface.",
  )
}

fn parse_tasks_from(
  node: yay.Node,
  env: Env,
) -> Result(config_types.LinearTaskScope, error.ConfigError) {
  case node {
    yay.NodeMap(pairs) -> {
      use scope <- result.try(parse_task_scope_predicate(
        pairs,
        env,
        "tracker.linear.tasks_from",
        1,
      ))
      enforce_task_scope_predicate_bounds(scope)
    }
    _ -> Error(error.InvalidConfig("tracker.linear.tasks_from must be a map"))
  }
}

fn parse_task_scope_predicate(
  pairs: List(#(yay.Node, yay.Node)),
  env: Env,
  path: String,
  depth: Int,
) -> Result(config_types.LinearTaskScope, error.ConfigError) {
  case depth > linear_task_scope.max_predicate_depth {
    True ->
      Error(error.InvalidConfig(
        "tracker.linear.tasks_from exceeds max predicate depth "
        <> int.to_string(linear_task_scope.max_predicate_depth)
        <> "; got "
        <> int.to_string(depth),
      ))
    False ->
      case pairs {
        [#(yay.NodeStr("project"), value)] ->
          parse_tasks_from_project_value(value, env, path <> ".project")
        [#(yay.NodeStr("projects"), value)] ->
          parse_tasks_from_projects_value(value, env, path <> ".projects")
        [#(yay.NodeStr("and"), value)] ->
          parse_tasks_from_boolean_value(
            value,
            env,
            path <> ".and",
            depth,
            config_types.LinearTaskAnd,
          )
        [#(yay.NodeStr("or"), value)] ->
          parse_tasks_from_boolean_value(
            value,
            env,
            path <> ".or",
            depth,
            config_types.LinearTaskOr,
          )
        [#(yay.NodeStr(key), _)] -> Error(unsupported_tasks_from_key(path, key))
        [#(_, _)] -> Error(error.InvalidConfig(path <> " keys must be strings"))
        [] ->
          Error(error.InvalidConfig(
            path <> " must contain exactly one key: project, projects, and, or",
          ))
        [_, ..] ->
          Error(error.InvalidConfig(
            path
            <> " must contain exactly one key; found "
            <> int.to_string(list.length(pairs))
            <> " keys at "
            <> path,
          ))
      }
  }
}

fn enforce_task_scope_predicate_bounds(
  scope: config_types.LinearTaskScope,
) -> Result(config_types.LinearTaskScope, error.ConfigError) {
  let stats = linear_task_scope.stats(scope)
  case linear_task_scope.is_anchored(scope) {
    False ->
      Error(error.InvalidConfig(
        "tracker.linear.tasks_from is unanchored. Add project/projects bounds or use a future explicit workspace-wide opt-in when available.",
      ))
    True ->
      case stats.max_depth > linear_task_scope.max_predicate_depth {
        True ->
          Error(error.InvalidConfig(
            "tracker.linear.tasks_from exceeds max predicate depth "
            <> int.to_string(linear_task_scope.max_predicate_depth)
            <> "; got "
            <> int.to_string(stats.max_depth),
          ))
        False ->
          case stats.predicate_nodes > linear_task_scope.max_predicate_nodes {
            True ->
              Error(error.InvalidConfig(
                "tracker.linear.tasks_from exceeds max predicate nodes "
                <> int.to_string(linear_task_scope.max_predicate_nodes)
                <> "; got "
                <> int.to_string(stats.predicate_nodes),
              ))
            False -> {
              use _ <- result.try(enforce_task_scope_project_slug_bound(scope))
              enforce_task_scope_issue_filter_payload_bound(scope)
            }
          }
      }
  }
}

fn enforce_task_scope_project_slug_bound(
  scope: config_types.LinearTaskScope,
) -> Result(config_types.LinearTaskScope, error.ConfigError) {
  let project_count = linear_task_scope.project_slugs(scope) |> list.length
  case project_count > linear_task_scope.max_array_entries {
    True ->
      Error(error.InvalidConfig(
        "tracker.linear.tasks_from references "
        <> int.to_string(project_count)
        <> " unique projects; maximum is "
        <> int.to_string(linear_task_scope.max_array_entries),
      ))
    False -> Ok(scope)
  }
}

fn enforce_task_scope_issue_filter_payload_bound(
  scope: config_types.LinearTaskScope,
) -> Result(config_types.LinearTaskScope, error.ConfigError) {
  let payload_bytes = linear_task_scope.issue_filter_payload_bytes(scope)
  case payload_bytes > linear_task_scope.max_issue_filter_payload_bytes {
    True ->
      Error(error.InvalidConfig(
        "tracker.linear.tasks_from compiled Linear IssueFilter payload is "
        <> int.to_string(payload_bytes)
        <> " bytes; maximum is "
        <> int.to_string(linear_task_scope.max_issue_filter_payload_bytes),
      ))
    False -> Ok(scope)
  }
}

fn parse_tasks_from_project_value(
  value: yay.Node,
  env: Env,
  path: String,
) -> Result(config_types.LinearTaskScope, error.ConfigError) {
  case value {
    yay.NodeStr(project) ->
      resolve_task_scope_project_value(project, path, env, fn(slug) {
        config_types.LinearTaskProject(slug)
      })
    _ -> Error(error.InvalidConfig(path <> " must be a string"))
  }
}

fn parse_tasks_from_projects_value(
  value: yay.Node,
  env: Env,
  path: String,
) -> Result(config_types.LinearTaskScope, error.ConfigError) {
  case value {
    yay.NodeSeq(values) -> {
      use _ <- result.try(validate_task_scope_array(values, path, "project"))
      use projects <- result.try(
        read_tasks_from_project_values(values, env, path, 0, []),
      )
      Ok(config_types.LinearTaskProjects(dedupe_preserving_first(projects)))
    }
    _ -> Error(error.InvalidConfig(path <> " must be a string list"))
  }
}

fn parse_tasks_from_boolean_value(
  value: yay.Node,
  env: Env,
  path: String,
  depth: Int,
  wrap: fn(List(config_types.LinearTaskScope)) -> config_types.LinearTaskScope,
) -> Result(config_types.LinearTaskScope, error.ConfigError) {
  case value {
    yay.NodeSeq(values) -> {
      use _ <- result.try(validate_task_scope_array(values, path, "predicate"))
      use children <- result.try(
        read_task_scope_predicates(values, env, path, depth, 0, []),
      )
      Ok(wrap(children))
    }
    _ -> Error(error.InvalidConfig(path <> " must be a predicate list"))
  }
}

fn validate_task_scope_array(
  values: List(yay.Node),
  path: String,
  item_name: String,
) -> Result(Nil, error.ConfigError) {
  let count = list.length(values)
  case count == 0 {
    True ->
      Error(error.InvalidConfig(
        path <> " must contain at least one " <> item_name,
      ))
    False ->
      case count > linear_task_scope.max_array_entries {
        True ->
          Error(error.InvalidConfig(
            path
            <> " has "
            <> int.to_string(count)
            <> " entries; maximum is "
            <> int.to_string(linear_task_scope.max_array_entries),
          ))
        False -> Ok(Nil)
      }
  }
}

fn read_task_scope_predicates(
  values: List(yay.Node),
  env: Env,
  path: String,
  depth: Int,
  index: Int,
  acc: List(config_types.LinearTaskScope),
) -> Result(List(config_types.LinearTaskScope), error.ConfigError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [yay.NodeMap(pairs), ..rest] -> {
      let child_path = path <> "[" <> int.to_string(index) <> "]"
      use child <- result.try(parse_task_scope_predicate(
        pairs,
        env,
        child_path,
        depth + 1,
      ))
      read_task_scope_predicates(rest, env, path, depth, index + 1, [
        child,
        ..acc
      ])
    }
    [_, ..] ->
      Error(error.InvalidConfig(
        path <> "[" <> int.to_string(index) <> "] must be a map",
      ))
  }
}

fn read_tasks_from_project_values(
  values: List(yay.Node),
  env: Env,
  path: String,
  index: Int,
  acc: List(String),
) -> Result(List(String), error.ConfigError) {
  let item_path = path <> "[" <> int.to_string(index) <> "]"
  case values {
    [] -> Ok(list.reverse(acc))
    [yay.NodeStr(value), ..rest] -> {
      use project <- result.try(
        resolve_task_scope_project_value(value, item_path, env, fn(slug) {
          slug
        }),
      )
      read_tasks_from_project_values(rest, env, path, index + 1, [
        project,
        ..acc
      ])
    }
    [_, ..] -> Error(error.InvalidConfig(item_path <> " must be a string"))
  }
}

fn resolve_task_scope_project_value(
  value: String,
  path: String,
  env: Env,
  wrap: fn(String) -> a,
) -> Result(a, error.ConfigError) {
  case resolve_optional_env(Some(value), env) {
    Some(project) -> validate_project_slug(project, path, wrap)
    None -> Error(error.InvalidConfig(path <> " must resolve to a string"))
  }
}

fn validate_project_slug(
  value: String,
  path: String,
  wrap: fn(String) -> a,
) -> Result(a, error.ConfigError) {
  case has_control_character(value) {
    True ->
      Error(error.InvalidConfig(path <> " must not contain control characters"))
    False -> {
      let value = string.trim(value)
      case value == "" {
        True -> Error(error.InvalidConfig(path <> " must be non-empty"))
        False -> validate_task_scope_scalar_length(value, path, wrap)
      }
    }
  }
}

fn validate_task_scope_scalar_length(
  value: String,
  path: String,
  wrap: fn(String) -> a,
) -> Result(a, error.ConfigError) {
  let scalar_count = unicode_scalar_length(value)
  case scalar_count > linear_task_scope.max_scalar_length {
    True ->
      Error(error.InvalidConfig(
        path
        <> " must be at most "
        <> int.to_string(linear_task_scope.max_scalar_length)
        <> " Unicode scalar values; got "
        <> int.to_string(scalar_count),
      ))
    False -> Ok(wrap(value))
  }
}

fn primary_project_slug(scope: config_types.LinearTaskScope) -> Option(String) {
  case scope {
    config_types.LinearTaskProject(project) -> Some(project)
    config_types.LinearTaskProjects(_)
    | config_types.LinearTaskAnd(_)
    | config_types.LinearTaskOr(_) -> None
  }
}

fn unsupported_tasks_from_key(path: String, key: String) -> error.ConfigError {
  let path = path <> "." <> key
  case key {
    "all_labels" | "any_label" ->
      error.InvalidConfig(
        path
        <> " is recognized for future label task-scope matching but is not enabled by this Scherzo build",
      )
    _ ->
      error.InvalidConfig(
        path
        <> " is not supported by this Scherzo build; supported keys are project, projects, and, or",
      )
  }
}

fn dedupe_preserving_first(values: List(String)) -> List(String) {
  dedupe_loop(values, []) |> list.reverse
}

fn dedupe_loop(values: List(String), acc: List(String)) -> List(String) {
  case values {
    [] -> acc
    [value, ..rest] -> {
      case list.contains(acc, value) {
        True -> dedupe_loop(rest, acc)
        False -> dedupe_loop(rest, [value, ..acc])
      }
    }
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

@external(erlang, "scherzo_config_ffi", "has_control_character")
fn has_control_character(value: String) -> Bool

@external(erlang, "scherzo_config_ffi", "unicode_scalar_length")
fn unicode_scalar_length(value: String) -> Int
