import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/tracker/state as issue_state
import yay

const simplified_schema_doc = "docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md"

pub type SimplifiedLinearContractFields {
  SimplifiedLinearContractFields(
    check_setup: Option(Bool),
    support_labels: Option(List(String)),
    invalid_workflow_state: Option(String),
    comment_on_invalid_workflow: Option(Bool),
  )
}

pub fn reject_removed_keys(root: yay.Node) -> Result(Nil, error.ConfigError) {
  use _ <- result.try(reject_removed_routing_keys(root))
  use _ <- result.try(reject_removed_polling_keys(root))
  use _ <- result.try(reject_removed_handoff_keys(root))
  reject_removed_tracker_state_keys(root)
}

pub fn resolve_routing(
  root: yay.Node,
  config_path: String,
) -> Result(config_types.RoutingConfig, error.ConfigError) {
  use labels <- result.try(task_routing_labels(root))
  use prefix_option <- result.try(get_string_strict(
    labels,
    "prefix",
    "task_routing.labels.prefix",
  ))
  let prefix =
    prefix_option
    |> option.unwrap("workflow:")
    |> normalize_label
  use require_exactly_one_option <- result.try(get_bool_strict(
    labels,
    "require_exactly_one",
    "task_routing.labels.require_exactly_one",
  ))
  let require_exactly_one =
    require_exactly_one_option
    |> bool_default(True)
  let default_workflow =
    get_non_empty_string(labels, "default_workflow")
    |> option.map(normalize_label)
  use _ <- result.try(validate_task_routing_label_policy(
    prefix,
    require_exactly_one,
  ))
  use workflows <- result.try(read_workflows(root, config_path))
  use _ <- result.try(validate_default_workflow(default_workflow, workflows))
  Ok(config_types.RoutingConfig(
    workflow_label_prefix: prefix,
    require_exactly_one_workflow_label: require_exactly_one,
    default_workflow: default_workflow,
    workflows: workflows,
  ))
}

pub fn resolve_linear_contract_fields(
  root: yay.Node,
) -> Result(SimplifiedLinearContractFields, error.ConfigError) {
  let tracker = get_map(root, "tracker")
  let linear = get_map(tracker, "linear")
  use linear_labels <- result.try(get_map_strict_or_empty(
    linear,
    "labels",
    "tracker.linear.labels",
  ))
  use check_setup <- result.try(get_bool_strict(
    linear,
    "check_setup",
    "tracker.linear.check_setup",
  ))
  use support_labels <- result.try(get_string_list(
    linear_labels,
    "support",
    "tracker.linear.labels.support",
  ))
  use task_labels <- result.try(task_routing_labels(root))
  use on_invalid <- result.try(get_map_strict_or_empty(
    task_labels,
    "on_invalid",
    "task_routing.labels.on_invalid",
  ))
  use invalid_workflow_state <- result.try(get_optional_string_strict(
    on_invalid,
    "state",
    "task_routing.labels.on_invalid.state",
  ))
  use comment_on_invalid_workflow <- result.try(get_bool_strict(
    on_invalid,
    "comment",
    "task_routing.labels.on_invalid.comment",
  ))
  Ok(SimplifiedLinearContractFields(
    check_setup: check_setup,
    support_labels: support_labels |> option.map(normalize_label_list),
    invalid_workflow_state: invalid_workflow_state |> optional_non_empty_string,
    comment_on_invalid_workflow: comment_on_invalid_workflow,
  ))
}

pub fn apply_linear_contract_fields(
  contract: config_types.LinearContractConfig,
  fields: SimplifiedLinearContractFields,
) -> config_types.LinearContractConfig {
  let SimplifiedLinearContractFields(
    check_setup: check_setup,
    support_labels: support_labels,
    invalid_workflow_state: invalid_workflow_state,
    comment_on_invalid_workflow: comment_on_invalid_workflow,
  ) = fields
  config_types.LinearContractConfig(
    ..contract,
    enabled: check_setup |> bool_default(contract.enabled),
    support_labels: support_labels |> option.unwrap(contract.support_labels),
    invalid_workflow_state_id: invalid_workflow_state
      |> option.lazy_or(fn() { contract.invalid_workflow_state_id }),
    invalid_workflow_state_target: case invalid_workflow_state {
      Some(value) -> Some(config_types.InvalidWorkflowStateName(value))
      None -> contract.invalid_workflow_state_target
    },
    comment_on_invalid_workflow: comment_on_invalid_workflow
      |> bool_default(contract.comment_on_invalid_workflow),
  )
}

pub fn resolve_orchestrator_linear_contract(
  root: yay.Node,
  effective: config_types.EffectiveConfig,
  routing: config_types.RoutingConfig,
  scheduled_jobs: List(config_types.ScheduledJobConfig),
) -> Result(config_types.LinearContractConfig, error.ConfigError) {
  let contract_result =
    config_types.resolve_linear_contract_for_routing(
      effective.linear_contract,
      routing,
      scheduled_jobs,
      linear_contract_field_present(root, "workflow_labels"),
      linear_contract_field_present(root, "workflow_label_prefix"),
    )
  use contract <- result.try(
    result.map_error(contract_result, fn(err) {
      error.InvalidConfig(config_types.linear_contract_routing_error_message(
        err,
      ))
    }),
  )
  Ok(
    contract
    |> apply_task_routing_enforcement(root, routing)
    |> derive_linear_contract_ready_state(effective.tracker),
  )
}

fn migration_hint(old_path: String, replacement: String) -> error.ConfigError {
  error.InvalidConfig(
    old_path
    <> " was removed. Use "
    <> replacement
    <> ". See "
    <> simplified_schema_doc
    <> ".",
  )
}

fn reject_removed_routing_keys(
  root: yay.Node,
) -> Result(Nil, error.ConfigError) {
  case get_node(root, "routing") {
    None -> Ok(Nil)
    Some(routing) ->
      case get_node(routing, "workflows") {
        Some(_) ->
          Error(migration_hint("routing.workflows", "top-level workflows"))
        None ->
          case get_node(routing, "workflow_label_prefix") {
            Some(_) ->
              Error(migration_hint(
                "routing.workflow_label_prefix",
                "task_routing.labels.prefix",
              ))
            None ->
              case get_node(routing, "require_exactly_one_workflow_label") {
                Some(_) ->
                  Error(migration_hint(
                    "routing.require_exactly_one_workflow_label",
                    "task_routing.labels.require_exactly_one",
                  ))
                None ->
                  case get_node(routing, "default_workflow") {
                    Some(_) ->
                      Error(migration_hint(
                        "routing.default_workflow",
                        "task_routing.labels.default_workflow",
                      ))
                    None ->
                      Error(migration_hint(
                        "routing",
                        "top-level workflows and task_routing.labels",
                      ))
                  }
              }
          }
      }
  }
}

fn reject_removed_polling_keys(
  root: yay.Node,
) -> Result(Nil, error.ConfigError) {
  case get_node(root, "polling") {
    None -> Ok(Nil)
    Some(polling) ->
      case get_node(polling, "interval_ms") {
        Some(_) ->
          Error(migration_hint(
            "polling.interval_ms",
            "tracker.polling.every with a duration string such as 30s",
          ))
        None ->
          case get_node(polling, "interval") {
            Some(_) ->
              Error(migration_hint("polling.interval", "tracker.polling.every"))
            None -> Error(migration_hint("polling", "tracker.polling"))
          }
      }
  }
}

fn reject_removed_handoff_keys(
  root: yay.Node,
) -> Result(Nil, error.ConfigError) {
  case get_node(root, "handoff") {
    None -> Ok(Nil)
    Some(yay.NodeMap(_) as handoff) -> reject_removed_handoff_map(handoff)
    Some(_) -> Error(migration_hint("handoff", "task_updates"))
  }
}

fn reject_removed_handoff_map(
  handoff: yay.Node,
) -> Result(Nil, error.ConfigError) {
  case get_node(handoff, "comment_on_claim") {
    Some(_) ->
      Error(migration_hint(
        "handoff.comment_on_claim",
        "task_updates.comment_on: [claim]",
      ))
    None ->
      case get_node(handoff, "comment_on_success") {
        Some(_) ->
          Error(migration_hint(
            "handoff.comment_on_success",
            "task_updates.comment_on: [success]",
          ))
        None ->
          case get_node(handoff, "comment_on_failure") {
            Some(_) ->
              Error(migration_hint(
                "handoff.comment_on_failure",
                "task_updates.comment_on: [failure]",
              ))
            None -> reject_removed_handoff_map_after_comments(handoff)
          }
      }
  }
}

fn reject_removed_handoff_map_after_comments(
  handoff: yay.Node,
) -> Result(Nil, error.ConfigError) {
  case get_node(handoff, "comment_on_park") {
    Some(_) ->
      Error(migration_hint(
        "handoff.comment_on_park",
        "task_updates.comment_on: [park]",
      ))
    None ->
      case get_node(handoff, "claim_state_id") {
        Some(_) ->
          Error(migration_hint(
            "handoff.claim_state_id",
            "task_updates.states.claim using the state name",
          ))
        None ->
          case get_node(handoff, "success_state_id") {
            Some(_) ->
              Error(migration_hint(
                "handoff.success_state_id",
                "task_updates.states.success using the state name",
              ))
            None -> reject_removed_handoff_state_tail(handoff)
          }
      }
  }
}

fn reject_removed_handoff_state_tail(
  handoff: yay.Node,
) -> Result(Nil, error.ConfigError) {
  case get_node(handoff, "failure_state_id") {
    Some(_) ->
      Error(migration_hint(
        "handoff.failure_state_id",
        "task_updates.states.failure using the state name",
      ))
    None ->
      case get_node(handoff, "completion_states") {
        Some(_) ->
          Error(migration_hint(
            "handoff.completion_states",
            "task_updates.states.success and task_updates.states.no_review_success using state names",
          ))
        None -> reject_removed_handoff_result_keys(handoff)
      }
  }
}

fn reject_removed_handoff_result_keys(
  handoff: yay.Node,
) -> Result(Nil, error.ConfigError) {
  case get_node(handoff, "include_result_on_success") {
    Some(_) ->
      Error(migration_hint(
        "handoff.include_result_on_success",
        "task_updates.result.on_success: comment",
      ))
    None ->
      case get_node(handoff, "attach_result_on_success") {
        Some(_) ->
          Error(migration_hint(
            "handoff.attach_result_on_success",
            "task_updates.result.on_success: attachment",
          ))
        None -> reject_removed_handoff_result_tail(handoff)
      }
  }
}

fn reject_removed_handoff_result_tail(
  handoff: yay.Node,
) -> Result(Nil, error.ConfigError) {
  case get_node(handoff, "result_max_chars") {
    Some(_) ->
      Error(migration_hint(
        "handoff.result_max_chars",
        "task_updates.result.max_chars",
      ))
    None ->
      case get_node(handoff, "enabled") {
        Some(_) ->
          Error(migration_hint("handoff.enabled", "task_updates.enabled"))
        None -> Error(migration_hint("handoff", "task_updates"))
      }
  }
}

fn reject_removed_tracker_state_keys(
  root: yay.Node,
) -> Result(Nil, error.ConfigError) {
  let tracker = get_map(root, "tracker")
  case get_node(tracker, "dispatch_states") {
    Some(_) ->
      Error(migration_hint("tracker.dispatch_states", "tracker.states.ready"))
    None ->
      case get_node(tracker, "active_states") {
        Some(_) ->
          Error(migration_hint("tracker.active_states", "tracker.states.active"))
        None ->
          case get_node(tracker, "terminal_states") {
            Some(_) ->
              Error(migration_hint(
                "tracker.terminal_states",
                "tracker.states.terminal",
              ))
            None -> Ok(Nil)
          }
      }
  }
}

fn task_routing_labels(root: yay.Node) -> Result(yay.Node, error.ConfigError) {
  use task_routing <- result.try(get_map_strict_or_empty(
    root,
    "task_routing",
    "task_routing",
  ))
  get_map_strict_or_empty(task_routing, "labels", "task_routing.labels")
}

fn validate_task_routing_label_policy(
  prefix: String,
  require_exactly_one: Bool,
) -> Result(Nil, error.ConfigError) {
  case require_exactly_one && prefix == "" {
    True ->
      Error(error.InvalidConfig(
        "task_routing.labels.prefix must be non-empty when task_routing.labels.require_exactly_one is true",
      ))
    False -> Ok(Nil)
  }
}

fn validate_default_workflow(
  default_workflow: Option(String),
  workflows: dict.Dict(String, String),
) -> Result(Nil, error.ConfigError) {
  case default_workflow {
    None -> Ok(Nil)
    Some(workflow_id) ->
      case valid_workflow_name(workflow_id) {
        False ->
          Error(error.InvalidConfig(
            "task_routing.labels.default_workflow has invalid workflow id: "
            <> workflow_id,
          ))
        True ->
          case dict.has_key(workflows, workflow_id) {
            True -> Ok(Nil)
            False ->
              Error(error.InvalidConfig(
                "task_routing.labels.default_workflow references unknown workflow: "
                <> workflow_id,
              ))
          }
      }
  }
}

fn read_workflows(
  root: yay.Node,
  config_path: String,
) -> Result(dict.Dict(String, String), error.ConfigError) {
  case get_node(root, "workflows") {
    None ->
      Error(error.InvalidConfig(
        "workflows is required. Move routing.workflows to top-level workflows. See docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md.",
      ))
    Some(yay.NodeMap(entries)) -> {
      use workflows <- result.try(
        read_workflow_entries(entries, config_path, []),
      )
      case dict.size(workflows) == 0 {
        True ->
          Error(error.InvalidConfig(
            "workflows must contain at least one workflow",
          ))
        False -> Ok(workflows)
      }
    }
    Some(_) -> Error(error.InvalidConfig("workflows must be a map"))
  }
}

fn read_workflow_entries(
  entries: List(#(yay.Node, yay.Node)),
  config_path: String,
  acc: List(#(String, String)),
) -> Result(dict.Dict(String, String), error.ConfigError) {
  case entries {
    [] -> Ok(dict.from_list(list.reverse(acc)))
    [#(yay.NodeStr(key), yay.NodeStr(value)), ..rest] -> {
      let workflow_id = normalize_label(key)
      case valid_workflow_name(workflow_id) {
        False ->
          Error(error.InvalidConfig(
            "workflows has invalid workflow id: " <> key,
          ))
        True -> {
          use path <- result.try(resolve_relative_config_path(
            value,
            config_path,
            "workflows." <> key,
          ))
          read_workflow_entries(rest, config_path, [#(workflow_id, path), ..acc])
        }
      }
    }
    [#(yay.NodeStr(_), _), ..] ->
      Error(error.InvalidConfig("workflows values must be strings"))
    [#(_, _), ..] ->
      Error(error.InvalidConfig("workflows keys must be strings"))
  }
}

fn apply_task_routing_enforcement(
  contract: config_types.LinearContractConfig,
  root: yay.Node,
  routing: config_types.RoutingConfig,
) -> config_types.LinearContractConfig {
  case linear_contract_field_present(root, "enforce_issue_workflow_labels") {
    True -> contract
    False ->
      config_types.LinearContractConfig(
        ..contract,
        enforce_issue_workflow_labels: routing.require_exactly_one_workflow_label,
      )
  }
}

fn derive_linear_contract_ready_state(
  contract: config_types.LinearContractConfig,
  tracker: config_types.TrackerConfig,
) -> config_types.LinearContractConfig {
  case dict.has_key(contract.required_states, "ready") {
    True -> contract
    False ->
      case tracker.dispatch_states {
        [] -> contract
        [ready, ..] ->
          config_types.LinearContractConfig(
            ..contract,
            required_states: dict.insert(
              contract.required_states,
              "ready",
              issue_state.to_string(ready),
            ),
          )
      }
  }
}

fn linear_contract_field_present(root: yay.Node, key: String) -> Bool {
  case get_node(get_map(root, "linear_contract"), key) {
    Some(_) -> True
    None -> False
  }
}

fn resolve_relative_config_path(
  value: String,
  config_path: String,
  field: String,
) -> Result(String, error.ConfigError) {
  let trimmed = string.trim(value)
  case trimmed == "" {
    True -> Error(error.InvalidConfig(field <> " must be non-empty"))
    False ->
      case
        string.starts_with(trimmed, "/")
        || string.starts_with(trimmed, "~/")
        || has_parent_segment(trimmed)
      {
        True ->
          Error(error.InvalidConfig(
            field <> " must be a relative path without .. or home expansion",
          ))
        False -> Ok(resolve_path(trimmed, config_path))
      }
  }
}

fn has_parent_segment(value: String) -> Bool {
  value == ".."
  || string.starts_with(value, "../")
  || string.ends_with(value, "/..")
  || string.contains(value, "/../")
}

fn valid_workflow_name(value: String) -> Bool {
  case string.to_graphemes(value) {
    [] -> False
    [first, ..rest] -> is_lower_or_digit(first) && all(rest, is_workflow_char)
  }
}

fn is_workflow_char(ch: String) -> Bool {
  is_lower_or_digit(ch) || ch == "_" || ch == "-"
}

fn is_lower_or_digit(ch: String) -> Bool {
  is_between(ch, "a", "z") || is_between(ch, "0", "9")
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

fn normalize_label(value: String) -> String {
  value |> string.trim |> string.lowercase
}

fn normalize_label_list(values: List(String)) -> List(String) {
  values
  |> list.map(normalize_label)
  |> list.filter(fn(value) { value != "" })
  |> dedupe_preserving_first
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
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(value) -> Some(value)
        Error(Nil) -> None
      }
    _ -> None
  }
}

fn get_string(node: yay.Node, key: String) -> Option(String) {
  case get_node(node, key) {
    Some(yay.NodeStr(value)) -> Some(value)
    _ -> None
  }
}

fn get_non_empty_string(node: yay.Node, key: String) -> Option(String) {
  case get_string(node, key) {
    Some(value) -> {
      let value = string.trim(value)
      case value == "" {
        True -> None
        False -> Some(value)
      }
    }
    None -> None
  }
}

fn get_bool_strict(
  node: yay.Node,
  key: String,
  path: String,
) -> Result(Option(Bool), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeBool(value)) -> Ok(Some(value))
    Some(_) -> Error(error.InvalidConfig(path <> " must be a boolean"))
  }
}

fn get_string_strict(
  node: yay.Node,
  key: String,
  path: String,
) -> Result(Option(String), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeStr(value)) -> Ok(Some(value))
    Some(_) -> Error(error.InvalidConfig(path <> " must be a string"))
  }
}

fn get_optional_string_strict(
  node: yay.Node,
  key: String,
  path: String,
) -> Result(Option(String), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeNil) -> Ok(None)
    Some(yay.NodeStr(value)) -> Ok(Some(value))
    Some(_) -> Error(error.InvalidConfig(path <> " must be a string or null"))
  }
}

fn get_string_list(
  node: yay.Node,
  key: String,
  path: String,
) -> Result(Option(List(String)), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeSeq(values)) -> {
      use strings <- result.try(read_string_list(values, path, []))
      Ok(Some(list.reverse(strings)))
    }
    Some(_) -> Error(error.InvalidConfig(path <> " must be a string list"))
  }
}

fn read_string_list(
  values: List(yay.Node),
  path: String,
  acc: List(String),
) -> Result(List(String), error.ConfigError) {
  case values {
    [] -> Ok(acc)
    [yay.NodeStr(value), ..rest] -> read_string_list(rest, path, [value, ..acc])
    [_, ..] -> Error(error.InvalidConfig(path <> " entries must be strings"))
  }
}

fn optional_non_empty_string(value: Option(String)) -> Option(String) {
  case value {
    None -> None
    Some(value) -> {
      let value = string.trim(value)
      case value == "" {
        True -> None
        False -> Some(value)
      }
    }
  }
}

fn bool_default(value: Option(Bool), default: Bool) -> Bool {
  option.unwrap(value, default)
}

fn resolve_path(path: String, workflow_path: String) -> String {
  case string.starts_with(path, "~/") {
    True ->
      home()
      |> result.map(fn(home) {
        absname_or(home <> "/" <> string.drop_start(path, 2), path)
      })
      |> result.unwrap(path)
    False ->
      case string.starts_with(path, "/") {
        True -> absname_or(path, path)
        False -> {
          let dir = dirname(workflow_path) |> result.unwrap(".")
          absname_or(dir <> "/" <> path, path)
        }
      }
  }
}

fn absname_or(path: String, fallback: String) -> String {
  absname(path) |> result.unwrap(fallback)
}

@external(erlang, "scherzo_config_ffi", "home")
fn home() -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "dirname")
fn dirname(path: String) -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "absname")
fn absname(path: String) -> Result(String, Nil)
