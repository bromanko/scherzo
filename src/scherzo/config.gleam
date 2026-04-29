import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/domain
import scherzo/error
import yay

pub type ReloadStatus {
  CurrentValid
  CurrentInvalid(String)
}

pub type ReloadState {
  ReloadState(
    last_known_good: Option(domain.EffectiveConfig),
    current_status: ReloadStatus,
  )
}

pub type ReloadResult {
  ReloadResult(state: ReloadState, resolved_secrets: List(String))
}

pub type Env =
  fn(String) -> Option(String)

pub fn default_tracker_config() -> domain.TrackerConfig {
  domain.TrackerConfig(
    kind: "linear",
    endpoint: "https://api.linear.app/graphql",
    api_key: None,
    project_slug: None,
    active_states: ["Todo", "In Progress"],
    terminal_states: ["Closed", "Cancelled", "Canceled", "Duplicate", "Done"],
  )
}

pub fn default_polling_config() -> domain.PollingConfig {
  domain.PollingConfig(interval_ms: 30_000)
}

pub fn default_workspace_config(workflow_path: String) -> domain.WorkspaceConfig {
  let root = default_workspace_root(workflow_path)
  domain.WorkspaceConfig(root: root)
}

pub fn default_hooks_config() -> domain.HooksConfig {
  domain.HooksConfig(
    after_create: None,
    before_run: None,
    after_run: None,
    before_remove: None,
    timeout_ms: 60_000,
  )
}

pub fn default_agent_config() -> domain.AgentConfig {
  domain.AgentConfig(
    max_concurrent_agents: 10,
    max_turns: 20,
    max_retry_backoff_ms: 300_000,
    max_retry_attempts: 5,
    max_sessions_per_issue: 3,
    max_concurrent_agents_by_state: dict.new(),
  )
}

pub fn default_pi_config() -> domain.PiConfig {
  domain.PiConfig(
    command: "pi --mode rpc --no-session",
    turn_timeout_ms: 3_600_000,
    read_timeout_ms: 5000,
    stall_timeout_ms: 300_000,
    auto_retry: True,
    ui_request_policy: domain.Cancel,
    ui_request_timeout_ms: 300_000,
    compatibility_probe: True,
    rate_limit_payload: None,
  )
}

pub fn default_handoff_config() -> domain.HandoffConfig {
  domain.HandoffConfig(
    enabled: False,
    comment_on_claim: False,
    comment_on_success: False,
    comment_on_failure: False,
    claim_state_id: None,
    success_state_id: None,
    failure_state_id: None,
  )
}

pub fn default_linear_contract_config() -> domain.LinearContractConfig {
  domain.LinearContractConfig(
    enabled: False,
    workflow_label_prefix: "workflow:",
    workflow_labels: [],
    support_labels: [],
    required_states: dict.new(),
    handoff_state_bindings: dict.new(),
    enforce_issue_workflow_labels: False,
    invalid_workflow_state_id: None,
    comment_on_invalid_workflow: False,
  )
}

pub fn default_linear_command_config() -> domain.LinearCommandConfig {
  domain.LinearCommandConfig(
    enabled: False,
    prefix: "/scherzo",
    authorized_user_ids: [],
    poll_limit_per_issue: 25,
    max_comments_per_tick: 50,
    acknowledge_success: True,
    acknowledge_rejection: True,
  )
}

pub fn resolve(
  workflow: domain.WorkflowDefinition,
  workflow_path: String,
) -> Result(domain.EffectiveConfig, error.ConfigError) {
  resolve_with_env(workflow, workflow_path, real_env)
}

pub fn resolve_with_env(
  workflow: domain.WorkflowDefinition,
  workflow_path: String,
  env: Env,
) -> Result(domain.EffectiveConfig, error.ConfigError) {
  let root = workflow.config
  use tracker <- result_try(resolve_tracker(root, env))
  use polling <- result_try(resolve_polling(root))
  use workspace <- result_try(resolve_workspace(root, workflow_path, env))
  use hooks <- result_try(resolve_hooks(root))
  use agent <- result_try(resolve_agent(root))
  use pi <- result_try(resolve_pi(root))
  use handoff <- result_try(resolve_handoff(root))
  use linear_contract <- result_try(resolve_linear_contract(root))
  use linear_commands <- result_try(resolve_linear_commands(root))
  Ok(domain.EffectiveConfig(
    tracker:,
    polling:,
    workspace:,
    hooks:,
    agent:,
    pi:,
    handoff:,
    linear_contract:,
    linear_commands:,
  ))
}

pub fn validate_dispatch(
  config: domain.EffectiveConfig,
) -> Result(Nil, error.ConfigError) {
  case
    non_empty_option(config.hooks.after_create)
    || non_empty_option(config.hooks.before_run)
  {
    True -> Ok(Nil)
    False ->
      Error(error.DispatchValidationFailed(
        "hooks.after_create or hooks.before_run is required",
      ))
  }
}

pub fn can_dispatch(state: ReloadState) -> Bool {
  case state.current_status {
    CurrentValid -> True
    CurrentInvalid(_) -> False
  }
}

pub fn initial_reload_state() -> ReloadState {
  ReloadState(
    last_known_good: None,
    current_status: CurrentInvalid("not loaded"),
  )
}

pub fn apply_reload(
  state: ReloadState,
  workflow: domain.WorkflowDefinition,
  workflow_path: String,
  env: Env,
) -> ReloadResult {
  case resolve_with_env(workflow, workflow_path, env) {
    Ok(config) -> {
      let secrets = resolved_secrets(config)
      ReloadResult(
        state: ReloadState(
          last_known_good: Some(config),
          current_status: CurrentValid,
        ),
        resolved_secrets: secrets,
      )
    }
    Error(err) ->
      ReloadResult(
        state: ReloadState(
          last_known_good: state.last_known_good,
          current_status: CurrentInvalid(config_error_message(err)),
        ),
        resolved_secrets: [],
      )
  }
}

pub fn resolved_secrets(config: domain.EffectiveConfig) -> List(String) {
  case config.tracker.api_key {
    Some(value) -> [value]
    None -> []
  }
}

fn resolve_tracker(
  root: yay.Node,
  env: Env,
) -> Result(domain.TrackerConfig, error.ConfigError) {
  let tracker_node = get_map(root, "tracker")
  let kind =
    get_required_string(
      tracker_node,
      "kind",
      error.UnsupportedTrackerKind("missing"),
    )
  use kind <- result_try(kind)
  case string.lowercase(kind) {
    "linear" -> {
      let endpoint =
        get_string(tracker_node, "endpoint")
        |> option_unwrap("https://api.linear.app/graphql")
      use endpoint <- result_try(validate_https_endpoint(endpoint))
      let active_states =
        get_string_list(tracker_node, "active_states")
        |> list_default(["Todo", "In Progress"])
      let terminal_states =
        get_string_list(tracker_node, "terminal_states")
        |> list_default(["Closed", "Cancelled", "Canceled", "Duplicate", "Done"])
      let raw_api_key = get_string(tracker_node, "api_key")
      let api_key =
        resolve_optional_env(raw_api_key, env)
        |> option_or_else(fn() { env("LINEAR_API_KEY") })
      let project_slug =
        get_string(tracker_node, "project_slug")
        |> resolve_optional_env(env)
      use project_slug <- result_try(required_option(
        project_slug,
        error.MissingTrackerProjectSlug,
      ))
      use api_key <- result_try(required_option(
        api_key,
        error.MissingTrackerApiKey,
      ))
      Ok(domain.TrackerConfig(
        kind: "linear",
        endpoint: endpoint,
        api_key: Some(api_key),
        project_slug: Some(project_slug),
        active_states: active_states,
        terminal_states: terminal_states,
      ))
    }
    other -> Error(error.UnsupportedTrackerKind(other))
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

fn resolve_polling(
  root: yay.Node,
) -> Result(domain.PollingConfig, error.ConfigError) {
  let polling = get_map(root, "polling")
  let interval = get_int(polling, "interval_ms") |> int_default(30_000)
  case interval > 0 {
    True -> Ok(domain.PollingConfig(interval_ms: interval))
    False -> Error(error.InvalidConfig("polling.interval_ms must be positive"))
  }
}

fn resolve_workspace(
  root: yay.Node,
  workflow_path: String,
  env: Env,
) -> Result(domain.WorkspaceConfig, error.ConfigError) {
  let workspace = get_map(root, "workspace")
  let raw = get_string(workspace, "root")
  let root =
    raw
    |> resolve_optional_env(env)
    |> option_unwrap(default_workspace_root(workflow_path))
  Ok(domain.WorkspaceConfig(root: resolve_path(root, workflow_path)))
}

fn resolve_hooks(
  root: yay.Node,
) -> Result(domain.HooksConfig, error.ConfigError) {
  let hooks = get_map(root, "hooks")
  let timeout = get_int(hooks, "timeout_ms") |> int_default(60_000)
  case timeout > 0 {
    False -> Error(error.InvalidConfig("hooks.timeout_ms must be positive"))
    True -> {
      Ok(domain.HooksConfig(
        after_create: get_non_empty_string(hooks, "after_create"),
        before_run: get_non_empty_string(hooks, "before_run"),
        after_run: get_non_empty_string(hooks, "after_run"),
        before_remove: get_non_empty_string(hooks, "before_remove"),
        timeout_ms: timeout,
      ))
    }
  }
}

fn resolve_agent(
  root: yay.Node,
) -> Result(domain.AgentConfig, error.ConfigError) {
  let agent = get_map(root, "agent")
  let max_concurrent_agents =
    get_int(agent, "max_concurrent_agents") |> int_default(10)
  let max_turns = get_int(agent, "max_turns") |> int_default(20)
  let max_retry_backoff_ms =
    get_int(agent, "max_retry_backoff_ms") |> int_default(300_000)
  let max_retry_attempts =
    get_int(agent, "max_retry_attempts") |> int_default(5)
  let max_sessions_per_issue =
    get_int(agent, "max_sessions_per_issue") |> int_default(3)

  case max_concurrent_agents < 0 {
    True ->
      Error(error.InvalidConfig(
        "agent.max_concurrent_agents must be zero or positive",
      ))
    False ->
      case
        max_turns <= 0
        || max_retry_backoff_ms <= 0
        || max_retry_attempts <= 0
        || max_sessions_per_issue <= 0
      {
        True -> Error(error.InvalidConfig("agent limits must be positive"))
        False ->
          Ok(domain.AgentConfig(
            max_concurrent_agents: max_concurrent_agents,
            max_turns: max_turns,
            max_retry_backoff_ms: max_retry_backoff_ms,
            max_retry_attempts: max_retry_attempts,
            max_sessions_per_issue: max_sessions_per_issue,
            max_concurrent_agents_by_state: get_positive_int_map(
              agent,
              "max_concurrent_agents_by_state",
            ),
          ))
      }
  }
}

fn resolve_pi(root: yay.Node) -> Result(domain.PiConfig, error.ConfigError) {
  let pi = get_map(root, "pi")
  let command =
    get_string(pi, "command") |> option_unwrap("pi --mode rpc --no-session")
  let turn_timeout_ms = get_int(pi, "turn_timeout_ms") |> int_default(3_600_000)
  let read_timeout_ms = get_int(pi, "read_timeout_ms") |> int_default(5000)
  let stall_timeout_ms = get_int(pi, "stall_timeout_ms") |> int_default(300_000)
  let ui_request_timeout_ms =
    get_int(pi, "ui_request_timeout_ms") |> int_default(300_000)
  case
    string.trim(command) == ""
    || turn_timeout_ms <= 0
    || read_timeout_ms <= 0
    || stall_timeout_ms < 0
    || ui_request_timeout_ms <= 0
  {
    True -> Error(error.InvalidConfig("invalid pi config"))
    False -> {
      use ui_request_policy <- result_try(
        ui_policy(get_string(pi, "ui_request_policy")),
      )
      Ok(domain.PiConfig(
        command: command,
        turn_timeout_ms: turn_timeout_ms,
        read_timeout_ms: read_timeout_ms,
        stall_timeout_ms: stall_timeout_ms,
        auto_retry: get_bool(pi, "auto_retry") |> bool_default(True),
        ui_request_policy: ui_request_policy,
        ui_request_timeout_ms: ui_request_timeout_ms,
        compatibility_probe: get_bool(pi, "compatibility_probe")
          |> bool_default(True),
        rate_limit_payload: None,
      ))
    }
  }
}

fn resolve_handoff(
  root: yay.Node,
) -> Result(domain.HandoffConfig, error.ConfigError) {
  let handoff = get_map(root, "handoff")
  let enabled = get_bool(handoff, "enabled") |> bool_default(False)
  let default_comment = enabled
  Ok(domain.HandoffConfig(
    enabled: enabled,
    comment_on_claim: get_bool(handoff, "comment_on_claim")
      |> bool_default(default_comment),
    comment_on_success: get_bool(handoff, "comment_on_success")
      |> bool_default(default_comment),
    comment_on_failure: get_bool(handoff, "comment_on_failure")
      |> bool_default(default_comment),
    claim_state_id: get_non_empty_string(handoff, "claim_state_id"),
    success_state_id: get_non_empty_string(handoff, "success_state_id"),
    failure_state_id: get_non_empty_string(handoff, "failure_state_id"),
  ))
}

fn resolve_linear_contract(
  root: yay.Node,
) -> Result(domain.LinearContractConfig, error.ConfigError) {
  let defaults = default_linear_contract_config()
  case get_node(root, "linear_contract") {
    None -> Ok(defaults)
    Some(node) -> {
      case node {
        yay.NodeMap(_) -> {
          use enabled_option <- result_try(get_bool_strict(
            node,
            "enabled",
            "linear_contract.enabled",
          ))
          use prefix_option <- result_try(get_string_strict(
            node,
            "workflow_label_prefix",
            "linear_contract.workflow_label_prefix",
          ))
          use workflow_labels_option <- result_try(get_contract_string_list(
            node,
            "workflow_labels",
            "linear_contract.workflow_labels",
          ))
          use support_labels_option <- result_try(get_contract_string_list(
            node,
            "support_labels",
            "linear_contract.support_labels",
          ))
          use required_states_option <- result_try(get_contract_string_map(
            node,
            "required_states",
            "linear_contract.required_states",
            string.trim,
          ))
          use handoff_bindings_option <- result_try(
            get_contract_string_map(
              node,
              "handoff_state_bindings",
              "linear_contract.handoff_state_bindings",
              fn(value) { value |> string.trim |> string.lowercase },
            ),
          )
          use enforce_option <- result_try(get_bool_strict(
            node,
            "enforce_issue_workflow_labels",
            "linear_contract.enforce_issue_workflow_labels",
          ))
          use invalid_state_option <- result_try(get_optional_string_strict(
            node,
            "invalid_workflow_state_id",
            "linear_contract.invalid_workflow_state_id",
          ))
          use comment_option <- result_try(get_bool_strict(
            node,
            "comment_on_invalid_workflow",
            "linear_contract.comment_on_invalid_workflow",
          ))

          let enabled = enabled_option |> bool_default(defaults.enabled)
          let workflow_label_prefix =
            prefix_option
            |> option_unwrap(defaults.workflow_label_prefix)
            |> normalize_label
          let workflow_labels =
            workflow_labels_option
            |> list_default(defaults.workflow_labels)
            |> normalize_label_list
          let support_labels =
            support_labels_option
            |> list_default(defaults.support_labels)
            |> normalize_label_list
          let required_states =
            required_states_option
            |> option_unwrap(defaults.required_states)
          let handoff_state_bindings =
            handoff_bindings_option
            |> option_unwrap(defaults.handoff_state_bindings)
          let enforce_issue_workflow_labels =
            enforce_option
            |> bool_default(defaults.enforce_issue_workflow_labels)
          let invalid_workflow_state_id =
            invalid_state_option
            |> optional_non_empty_string
            |> option_or_else(fn() { defaults.invalid_workflow_state_id })
          let comment_on_invalid_workflow =
            comment_option
            |> bool_default(defaults.comment_on_invalid_workflow)
          use handoff_state_bindings <- result_try(validate_handoff_bindings(
            handoff_state_bindings,
            required_states,
          ))

          case
            validate_linear_contract_dispatch_policy(
              enabled,
              enforce_issue_workflow_labels,
              workflow_label_prefix,
              workflow_labels,
            )
          {
            Error(err) -> Error(err)
            Ok(Nil) ->
              Ok(domain.LinearContractConfig(
                enabled: enabled,
                workflow_label_prefix: workflow_label_prefix,
                workflow_labels: workflow_labels,
                support_labels: support_labels,
                required_states: required_states,
                handoff_state_bindings: handoff_state_bindings,
                enforce_issue_workflow_labels: enforce_issue_workflow_labels,
                invalid_workflow_state_id: invalid_workflow_state_id,
                comment_on_invalid_workflow: comment_on_invalid_workflow,
              ))
          }
        }
        _ -> Error(error.InvalidConfig("linear_contract must be a map"))
      }
    }
  }
}

fn resolve_linear_commands(
  root: yay.Node,
) -> Result(domain.LinearCommandConfig, error.ConfigError) {
  let node = get_map(root, "linear_commands")
  let defaults = default_linear_command_config()
  let enabled = get_bool(node, "enabled") |> bool_default(defaults.enabled)
  let prefix = get_string(node, "prefix") |> option_unwrap(defaults.prefix)
  let prefix = string.trim(prefix)
  let authorized_user_ids =
    get_string_list(node, "authorized_user_ids")
    |> list_default(defaults.authorized_user_ids)
    |> normalize_string_list
  let poll_limit_per_issue =
    get_int(node, "poll_limit_per_issue")
    |> int_default(defaults.poll_limit_per_issue)
  let max_comments_per_tick =
    get_int(node, "max_comments_per_tick")
    |> int_default(defaults.max_comments_per_tick)
  let acknowledge_success =
    get_bool(node, "acknowledge_success")
    |> bool_default(defaults.acknowledge_success)
  let acknowledge_rejection =
    get_bool(node, "acknowledge_rejection")
    |> bool_default(defaults.acknowledge_rejection)
  case prefix == "" {
    True ->
      Error(error.InvalidConfig("linear_commands.prefix must be non-empty"))
    False ->
      case poll_limit_per_issue <= 0 || max_comments_per_tick <= 0 {
        True ->
          Error(error.InvalidConfig(
            "linear_commands poll limits must be positive",
          ))
        False ->
          case enabled && list.is_empty(authorized_user_ids) {
            True ->
              Error(error.InvalidConfig(
                "linear_commands.authorized_user_ids is required when enabled",
              ))
            False ->
              Ok(domain.LinearCommandConfig(
                enabled: enabled,
                prefix: prefix,
                authorized_user_ids: authorized_user_ids,
                poll_limit_per_issue: poll_limit_per_issue,
                max_comments_per_tick: max_comments_per_tick,
                acknowledge_success: acknowledge_success,
                acknowledge_rejection: acknowledge_rejection,
              ))
          }
      }
  }
}

fn normalize_string_list(values: List(String)) -> List(String) {
  values
  |> list.map(string.trim)
  |> list.filter(fn(value) { value != "" })
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

fn validate_linear_contract_dispatch_policy(
  enabled: Bool,
  enforce_issue_workflow_labels: Bool,
  workflow_label_prefix: String,
  workflow_labels: List(String),
) -> Result(Nil, error.ConfigError) {
  case
    { enabled || enforce_issue_workflow_labels } && workflow_label_prefix == ""
  {
    True ->
      Error(error.InvalidConfig(
        "linear_contract.workflow_label_prefix must be non-empty when enabled or enforcing issue workflow labels",
      ))
    False ->
      case enforce_issue_workflow_labels && list.is_empty(workflow_labels) {
        True ->
          Error(error.InvalidConfig(
            "linear_contract.workflow_labels must be non-empty when enforce_issue_workflow_labels is true",
          ))
        False -> Ok(Nil)
      }
  }
}

fn validate_handoff_bindings(
  bindings: dict.Dict(String, String),
  required_states: dict.Dict(String, String),
) -> Result(dict.Dict(String, String), error.ConfigError) {
  validate_handoff_binding_entries(dict.to_list(bindings), required_states)
  |> result_map(fn(_) { bindings })
}

fn validate_handoff_binding_entries(
  entries: List(#(String, String)),
  required_states: dict.Dict(String, String),
) -> Result(Nil, error.ConfigError) {
  case entries {
    [] -> Ok(Nil)
    [#(key, value), ..rest] -> {
      case list.contains(["claim", "success", "failure"], key) {
        False ->
          Error(error.InvalidConfig(
            "linear_contract.handoff_state_bindings has invalid key: " <> key,
          ))
        True ->
          case dict.has_key(required_states, value) {
            False ->
              Error(error.InvalidConfig(
                "linear_contract.handoff_state_bindings."
                <> key
                <> " references unknown required state: "
                <> value,
              ))
            True -> validate_handoff_binding_entries(rest, required_states)
          }
      }
    }
  }
}

fn ui_policy(
  value: Option(String),
) -> Result(domain.UiRequestPolicy, error.ConfigError) {
  case value {
    Some(value) ->
      case string.lowercase(string.trim(value)) {
        "cancel" -> Ok(domain.Cancel)
        "fail" -> Ok(domain.Fail)
        "ignore" -> Ok(domain.Ignore)
        "operator" -> Ok(domain.Operator)
        other ->
          Error(error.InvalidConfig("invalid pi.ui_request_policy: " <> other))
      }
    None -> Ok(domain.Cancel)
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
        Error(_) -> None
      }
    _ -> None
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
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(yay.NodeStr(value)) -> Some(value)
        _ -> None
      }
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

fn get_int(node: yay.Node, key: String) -> Option(Int) {
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(yay.NodeInt(value)) -> Some(value)
        _ -> None
      }
    _ -> None
  }
}

fn get_bool(node: yay.Node, key: String) -> Option(Bool) {
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(yay.NodeBool(value)) -> Some(value)
        _ -> None
      }
    _ -> None
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

fn get_string_list(node: yay.Node, key: String) -> Option(List(String)) {
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(yay.NodeSeq(values)) -> {
          let strings =
            list.filter_map(values, fn(value) {
              case value {
                yay.NodeStr(s) -> Ok(s)
                _ -> Error(Nil)
              }
            })
          Some(strings)
        }
        _ -> None
      }
    _ -> None
  }
}

fn get_contract_string_list(
  node: yay.Node,
  key: String,
  path: String,
) -> Result(Option(List(String)), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeSeq(values)) -> {
      use strings <- result_try(read_contract_string_list(values, path, []))
      Ok(Some(list.reverse(strings)))
    }
    Some(_) -> Error(error.InvalidConfig(path <> " must be a string list"))
  }
}

fn read_contract_string_list(
  values: List(yay.Node),
  path: String,
  acc: List(String),
) -> Result(List(String), error.ConfigError) {
  case values {
    [] -> Ok(acc)
    [yay.NodeStr(value), ..rest] ->
      read_contract_string_list(rest, path, [value, ..acc])
    [_, ..] -> Error(error.InvalidConfig(path <> " entries must be strings"))
  }
}

fn get_contract_string_map(
  node: yay.Node,
  key: String,
  path: String,
  normalize_value: fn(String) -> String,
) -> Result(Option(dict.Dict(String, String)), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeMap(entries)) -> {
      use pairs <- result_try(
        read_contract_string_map(entries, path, normalize_value, []),
      )
      Ok(Some(dict.from_list(list.reverse(pairs))))
    }
    Some(_) -> Error(error.InvalidConfig(path <> " must be a string map"))
  }
}

fn read_contract_string_map(
  entries: List(#(yay.Node, yay.Node)),
  path: String,
  normalize_value: fn(String) -> String,
  acc: List(#(String, String)),
) -> Result(List(#(String, String)), error.ConfigError) {
  case entries {
    [] -> Ok(acc)
    [#(yay.NodeStr(key), yay.NodeStr(value)), ..rest] -> {
      let key = key |> string.trim |> string.lowercase
      let value = normalize_value(value)
      case key == "" || value == "" {
        True ->
          Error(error.InvalidConfig(
            path <> " keys and values must be non-empty strings",
          ))
        False ->
          read_contract_string_map(rest, path, normalize_value, [
            #(key, value),
            ..acc
          ])
      }
    }
    [#(yay.NodeStr(_), _), ..] ->
      Error(error.InvalidConfig(path <> " values must be strings"))
    [#(_, _), ..] -> Error(error.InvalidConfig(path <> " keys must be strings"))
  }
}

fn get_positive_int_map(node: yay.Node, key: String) -> dict.Dict(String, Int) {
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(yay.NodeMap(entries)) ->
          entries
          |> list.filter_map(fn(entry) {
            case entry {
              #(yay.NodeStr(k), yay.NodeInt(v)) if v > 0 ->
                Ok(#(string.lowercase(k), v))
              _ -> Error(Nil)
            }
          })
          |> dict.from_list
        _ -> dict.new()
      }
    _ -> dict.new()
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

fn resolve_path(path: String, workflow_path: String) -> String {
  case string.starts_with(path, "~/") {
    True ->
      case home() {
        Ok(home) ->
          absname(home <> "/" <> string.drop_start(path, 2))
          |> result_unwrap(path)
        Error(_) -> path
      }
    False ->
      case string.starts_with(path, "/") {
        True -> absname(path) |> result_unwrap(path)
        False -> {
          let dir = dirname(workflow_path) |> result_unwrap(".")
          absname(dir <> "/" <> path) |> result_unwrap(path)
        }
      }
  }
}

fn default_workspace_root(workflow_path: String) -> String {
  let tmp = tmpdir() |> result_unwrap("/tmp")
  let _ = workflow_path
  absname(tmp <> "/scherzo_workspaces")
  |> result_unwrap(tmp <> "/scherzo_workspaces")
}

fn real_env(name: String) -> Option(String) {
  case getenv(name) {
    Ok(value) -> Some(value)
    Error(_) -> None
  }
}

fn option_unwrap(value: Option(a), default: a) -> a {
  case value {
    Some(value) -> value
    None -> default
  }
}

fn option_or_else(value: Option(a), fallback: fn() -> Option(a)) -> Option(a) {
  case value {
    Some(_) -> value
    None -> fallback()
  }
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

fn non_empty_option(value: Option(String)) -> Bool {
  case value {
    Some(value) -> string.trim(value) != ""
    None -> False
  }
}

fn int_default(value: Option(Int), default: Int) -> Int {
  option_unwrap(value, default)
}

fn bool_default(value: Option(Bool), default: Bool) -> Bool {
  option_unwrap(value, default)
}

fn list_default(value: Option(List(a)), default: List(a)) -> List(a) {
  option_unwrap(value, default)
}

fn result_unwrap(result: Result(a, b), default: a) -> a {
  case result {
    Ok(value) -> value
    Error(_) -> default
  }
}

fn result_map(result: Result(a, e), mapper: fn(a) -> b) -> Result(b, e) {
  case result {
    Ok(value) -> Ok(mapper(value))
    Error(err) -> Error(err)
  }
}

fn config_error_message(err: error.ConfigError) -> String {
  error.config_code(err)
}

fn result_try(result: Result(a, e), next: fn(a) -> Result(b, e)) -> Result(b, e) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(err)
  }
}

@external(erlang, "scherzo_config_ffi", "getenv")
fn getenv(name: String) -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "home")
fn home() -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "tmpdir")
fn tmpdir() -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "dirname")
fn dirname(path: String) -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "absname")
fn absname(path: String) -> Result(String, Nil)
