import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import scherzo/config/types as config_types
import scherzo/error
import scherzo/model_config
import scherzo/orchestrator/schedule_core
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import yay

pub type ReloadStatus {
  CurrentValid
  CurrentInvalid(String)
}

pub type ReloadState {
  ReloadState(
    last_known_good: Option(config_types.EffectiveConfig),
    current_status: ReloadStatus,
  )
}

pub type ReloadResult {
  ReloadResult(state: ReloadState, resolved_secrets: List(String))
}

pub type Env =
  fn(String) -> Option(String)

pub fn default_tracker_config() -> config_types.TrackerConfig {
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

pub fn default_polling_config() -> config_types.PollingConfig {
  config_types.PollingConfig(interval_ms: 30_000)
}

pub fn default_workspace_config(
  workflow_path: String,
) -> config_types.WorkspaceConfig {
  let root = default_workspace_root(workflow_path)
  config_types.WorkspaceConfig(root: root)
}

pub fn default_hooks_config() -> config_types.HooksConfig {
  config_types.HooksConfig(
    after_create: None,
    before_run: None,
    after_run: None,
    before_remove: None,
    timeout_ms: 60_000,
  )
}

pub fn default_agent_config() -> config_types.AgentConfig {
  config_types.AgentConfig(
    max_concurrent_agents: 10,
    max_turns: 20,
    max_retry_backoff_ms: 300_000,
    max_retry_attempts: 5,
    max_sessions_per_issue: 3,
    max_concurrent_agents_by_state: dict.new(),
  )
}

pub fn default_recovery_prompt() -> String {
  "You are being resumed by Scherzo after an interrupted workflow agent step.\nContinue from the existing pi session context that was reopened for this step.\nDo not restart from scratch and do not assume the original prompt has been resent.\nWork in the current directory, which is the recorded workspace for this step.\nIf the prior session context shows the step was already completed, summarize the completed work and stop.\nOtherwise, inspect the current workspace as needed, finish the same step, and provide a concise final response for Scherzo."
}

pub fn default_pi_config() -> config_types.PiConfig {
  config_types.PiConfig(
    command: "pi --mode rpc --no-session",
    turn_timeout_ms: 3_600_000,
    read_timeout_ms: 5000,
    stall_timeout_ms: 300_000,
    auto_retry: True,
    ui_request_policy: config_types.Cancel,
    ui_request_timeout_ms: 300_000,
    compatibility_probe: True,
    rate_limit_payload: None,
    argv_command: None,
    session_persistence: config_types.PiSessionPersistenceConfig(
      enabled: False,
      recovery_prompt: default_recovery_prompt(),
    ),
  )
}

pub fn default_handoff_config() -> config_types.HandoffConfig {
  config_types.HandoffConfig(
    enabled: False,
    comment_on_claim: False,
    comment_on_success: False,
    comment_on_failure: False,
    comment_on_park: False,
    claim_state_id: None,
    success_state_id: None,
    failure_state_id: None,
    include_result_on_success: False,
    attach_result_on_success: False,
    attachment_fallback_to_markdown_link: True,
    result_max_chars: 8000,
  )
}

pub fn default_linear_contract_config() -> config_types.LinearContractConfig {
  config_types.LinearContractConfig(
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

pub fn default_linear_command_config() -> config_types.LinearCommandConfig {
  config_types.LinearCommandConfig(
    enabled: False,
    prefix: "/scherzo",
    authorized_user_ids: [],
    poll_limit_per_issue: 25,
    max_comments_per_tick: 50,
    acknowledge_success: True,
    acknowledge_rejection: True,
  )
}

pub fn default_scheduled_failure_config() -> config_types.ScheduledFailureConfig {
  config_types.ScheduledFailureConfig(
    linear: config_types.ScheduledLinearFailureConfig(
      enabled: False,
      state: None,
      labels: [],
      dedupe: config_types.OpenIssuePerJob,
    ),
  )
}

pub fn resolve_with_env(
  root: yay.Node,
  config_path: String,
  env: Env,
) -> Result(config_types.EffectiveConfig, error.ConfigError) {
  resolve_root(root, config_path, env)
}

pub fn resolve_root(
  root: yay.Node,
  config_path: String,
  env: Env,
) -> Result(config_types.EffectiveConfig, error.ConfigError) {
  use tracker <- result.try(resolve_tracker(root, env))
  use polling <- result.try(resolve_polling(root))
  use workspace <- result.try(resolve_workspace(root, config_path, env))
  use hooks <- result.try(resolve_hooks(root))
  use agent <- result.try(resolve_agent(root))
  use pi <- result.try(resolve_pi(root))
  use handoff <- result.try(resolve_handoff(root))
  use linear_contract <- result.try(resolve_linear_contract(root))
  use linear_commands <- result.try(resolve_linear_commands(root))
  Ok(config_types.EffectiveConfig(
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

pub fn resolve_orchestrator_root(
  root: yay.Node,
  config_path: String,
  env: Env,
) -> Result(config_types.OrchestratorConfig, error.ConfigError) {
  use effective <- result.try(resolve_root(root, config_path, env))
  use routing <- result.try(resolve_routing(root, config_path))
  use workspace_profiles <- result.try(resolve_workspace_profiles(root))
  use default_workspace_profile <- result.try(resolve_default_workspace_profile(
    workspace_profiles,
  ))
  let dag_hooks = default_workspace_profile.hooks
  use artifact_limits <- result.try(resolve_artifact_limits(root))
  use model_settings <- result.try(resolve_workflow_model_settings(root))
  use scheduled_jobs <- result.try(resolve_scheduled_jobs(root, routing))
  use linear_contract <- result.try(resolve_orchestrator_linear_contract(
    root,
    effective.linear_contract,
    routing,
  ))
  let effective = config_types.EffectiveConfig(..effective, linear_contract:)
  Ok(config_types.OrchestratorConfig(
    effective: effective,
    config_dir: dirname(config_path)
      |> result.unwrap(".")
      |> absname
      |> result.unwrap(dirname(config_path) |> result.unwrap(".")),
    routing: routing,
    dag_hooks: dag_hooks,
    workspace_profiles: workspace_profiles,
    artifact_limits: artifact_limits,
    model_settings: model_settings,
    scheduled_jobs: scheduled_jobs,
  ))
}

pub fn validate_dispatch(
  config: config_types.EffectiveConfig,
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
  root: yay.Node,
  config_path: String,
  env: Env,
) -> ReloadResult {
  case resolve_with_env(root, config_path, env) {
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

pub fn resolved_secrets(config: config_types.EffectiveConfig) -> List(String) {
  case config.tracker.api_key {
    Some(value) -> [value]
    None -> []
  }
}

fn resolve_tracker(
  root: yay.Node,
  env: Env,
) -> Result(config_types.TrackerConfig, error.ConfigError) {
  let tracker_node = get_map(root, "tracker")
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
        get_string(tracker_node, "endpoint")
        |> option.unwrap("https://api.linear.app/graphql")
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
      let raw_api_key = get_string(tracker_node, "api_key")
      let api_key =
        resolve_optional_env(raw_api_key, env)
        |> option.lazy_or(fn() { env("LINEAR_API_KEY") })
      let project_slug =
        get_string(tracker_node, "project_slug")
        |> resolve_optional_env(env)
      use project_slug <- result.try(required_option(
        project_slug,
        error.MissingTrackerProjectSlug,
      ))
      use api_key <- result.try(required_option(
        api_key,
        error.MissingTrackerApiKey,
      ))
      Ok(config_types.TrackerConfig(
        kind: kind,
        endpoint: endpoint,
        api_key: Some(api_key),
        project_slug: Some(project_slug),
        active_states: active_states,
        dispatch_states: dispatch_states,
        terminal_states: issue_state.list_from_strings(terminal_states),
      ))
    }
    Error(_) -> Error(error.UnsupportedTrackerKind(normalized_kind))
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

fn resolve_polling(
  root: yay.Node,
) -> Result(config_types.PollingConfig, error.ConfigError) {
  let polling = get_map(root, "polling")
  let interval = get_int(polling, "interval_ms") |> int_default(30_000)
  case interval > 0 {
    True -> Ok(config_types.PollingConfig(interval_ms: interval))
    False -> Error(error.InvalidConfig("polling.interval_ms must be positive"))
  }
}

fn resolve_workspace(
  root: yay.Node,
  workflow_path: String,
  env: Env,
) -> Result(config_types.WorkspaceConfig, error.ConfigError) {
  let workspace = get_map(root, "workspace")
  let raw = get_string(workspace, "root")
  let root =
    raw
    |> resolve_optional_env(env)
    |> option.unwrap(default_workspace_root(workflow_path))
  Ok(config_types.WorkspaceConfig(root: resolve_path(root, workflow_path)))
}

fn resolve_hooks(
  root: yay.Node,
) -> Result(config_types.HooksConfig, error.ConfigError) {
  let hooks = get_map(root, "hooks")
  let timeout = get_int(hooks, "timeout_ms") |> int_default(60_000)
  case timeout > 0 {
    False -> Error(error.InvalidConfig("hooks.timeout_ms must be positive"))
    True -> {
      Ok(config_types.HooksConfig(
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
) -> Result(config_types.AgentConfig, error.ConfigError) {
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
          Ok(config_types.AgentConfig(
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

fn resolve_pi(
  root: yay.Node,
) -> Result(config_types.PiConfig, error.ConfigError) {
  let pi = get_map(root, "pi")
  let command =
    get_string(pi, "command") |> option.unwrap("pi --mode rpc --no-session")
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
      use ui_request_policy <- result.try(
        ui_policy(get_string(pi, "ui_request_policy")),
      )
      use argv_command <- result.try(resolve_pi_argv(pi))
      use session_persistence <- result.try(resolve_pi_session_persistence(pi))
      use _ <- result.try(validate_pi_session_persistence(
        argv_command,
        session_persistence,
      ))
      Ok(config_types.PiConfig(
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
        argv_command: argv_command,
        session_persistence: session_persistence,
      ))
    }
  }
}

fn resolve_pi_argv(
  pi: yay.Node,
) -> Result(Option(config_types.PiArgvCommand), error.ConfigError) {
  case get_node(pi, "argv") {
    None -> Ok(None)
    Some(yay.NodeSeq(values)) -> {
      use argv <- result.try(read_string_values(values, "pi.argv", []))
      case argv {
        [] -> Error(error.InvalidConfig("pi.argv must be non-empty"))
        [executable, ..args] -> {
          use env <- result.try(get_string_map_strict(
            pi,
            "argv_env",
            "pi.argv_env",
          ))
          Ok(
            Some(config_types.PiArgvCommand(
              executable: executable,
              args: args,
              env: env,
            )),
          )
        }
      }
    }
    Some(_) -> Error(error.InvalidConfig("pi.argv must be a string list"))
  }
}

fn resolve_pi_session_persistence(
  pi: yay.Node,
) -> Result(config_types.PiSessionPersistenceConfig, error.ConfigError) {
  let defaults =
    config_types.PiSessionPersistenceConfig(
      enabled: False,
      recovery_prompt: default_recovery_prompt(),
    )
  case get_node(pi, "session_persistence") {
    None -> Ok(defaults)
    Some(node) ->
      case node {
        yay.NodeMap(_) -> {
          use enabled_option <- result.try(get_bool_strict(
            node,
            "enabled",
            "pi.session_persistence.enabled",
          ))
          use prompt_option <- result.try(get_optional_string_strict(
            node,
            "recovery_prompt",
            "pi.session_persistence.recovery_prompt",
          ))
          Ok(config_types.PiSessionPersistenceConfig(
            enabled: enabled_option |> bool_default(defaults.enabled),
            recovery_prompt: prompt_option
              |> optional_non_empty_string
              |> option.unwrap(defaults.recovery_prompt),
          ))
        }
        _ -> Error(error.InvalidConfig("pi.session_persistence must be a map"))
      }
  }
}

fn validate_pi_session_persistence(
  argv_command: Option(config_types.PiArgvCommand),
  session_persistence: config_types.PiSessionPersistenceConfig,
) -> Result(Nil, error.ConfigError) {
  case session_persistence.enabled {
    False -> Ok(Nil)
    True -> {
      use argv <- result.try(required_option(
        argv_command,
        error.InvalidConfig("pi.session_persistence requires pi.argv"),
      ))
      case string.trim(argv.executable) == "" {
        True ->
          Error(error.InvalidConfig(
            "pi.session_persistence requires pi.argv executable to be non-empty",
          ))
        False ->
          case has_forbidden_session_flag(argv.args) {
            True ->
              Error(error.InvalidConfig(
                "pi.session_persistence requires pi.argv without --session or --no-session",
              ))
            False -> Ok(Nil)
          }
      }
    }
  }
}

fn has_forbidden_session_flag(args: List(String)) -> Bool {
  list.any(args, fn(arg) { arg == "--session" || arg == "--no-session" })
}

fn resolve_handoff(
  root: yay.Node,
) -> Result(config_types.HandoffConfig, error.ConfigError) {
  let handoff = get_map(root, "handoff")
  let enabled = get_bool(handoff, "enabled") |> bool_default(False)
  let default_comment = enabled
  let comment_on_success =
    get_bool(handoff, "comment_on_success") |> bool_default(default_comment)
  let result_max_chars =
    get_int(handoff, "result_max_chars") |> int_default(8000)
  let attach_result_on_success =
    get_bool(handoff, "attach_result_on_success") |> bool_default(False)
  case result_max_chars <= 0 {
    True ->
      Error(error.InvalidConfig("handoff.result_max_chars must be positive"))
    False ->
      case attach_result_on_success && !comment_on_success {
        True ->
          Error(error.InvalidConfig(
            "handoff.attach_result_on_success requires handoff.comment_on_success to be true",
          ))
        False ->
          Ok(config_types.HandoffConfig(
            enabled: enabled,
            comment_on_claim: get_bool(handoff, "comment_on_claim")
              |> bool_default(default_comment),
            comment_on_success: comment_on_success,
            comment_on_failure: get_bool(handoff, "comment_on_failure")
              |> bool_default(default_comment),
            comment_on_park: get_bool(handoff, "comment_on_park")
              |> bool_default(default_comment),
            claim_state_id: get_non_empty_string(handoff, "claim_state_id"),
            success_state_id: get_non_empty_string(handoff, "success_state_id"),
            failure_state_id: get_non_empty_string(handoff, "failure_state_id"),
            include_result_on_success: get_bool(
              handoff,
              "include_result_on_success",
            )
              |> bool_default(comment_on_success),
            attach_result_on_success: attach_result_on_success,
            attachment_fallback_to_markdown_link: get_bool(
              handoff,
              "attachment_fallback_to_markdown_link",
            )
              |> bool_default(True),
            result_max_chars: result_max_chars,
          ))
      }
  }
}

fn resolve_linear_contract(
  root: yay.Node,
) -> Result(config_types.LinearContractConfig, error.ConfigError) {
  let defaults = default_linear_contract_config()
  case get_node(root, "linear_contract") {
    None -> Ok(defaults)
    Some(node) -> {
      case node {
        yay.NodeMap(_) -> {
          use enabled_option <- result.try(get_bool_strict(
            node,
            "enabled",
            "linear_contract.enabled",
          ))
          use prefix_option <- result.try(get_string_strict(
            node,
            "workflow_label_prefix",
            "linear_contract.workflow_label_prefix",
          ))
          use workflow_labels_option <- result.try(get_contract_string_list(
            node,
            "workflow_labels",
            "linear_contract.workflow_labels",
          ))
          use support_labels_option <- result.try(get_contract_string_list(
            node,
            "support_labels",
            "linear_contract.support_labels",
          ))
          use required_states_option <- result.try(get_contract_string_map(
            node,
            "required_states",
            "linear_contract.required_states",
            string.trim,
          ))
          use handoff_bindings_option <- result.try(
            get_contract_string_map(
              node,
              "handoff_state_bindings",
              "linear_contract.handoff_state_bindings",
              fn(value) { value |> string.trim |> string.lowercase },
            ),
          )
          use enforce_option <- result.try(get_bool_strict(
            node,
            "enforce_issue_workflow_labels",
            "linear_contract.enforce_issue_workflow_labels",
          ))
          use invalid_state_option <- result.try(get_optional_string_strict(
            node,
            "invalid_workflow_state_id",
            "linear_contract.invalid_workflow_state_id",
          ))
          use comment_option <- result.try(get_bool_strict(
            node,
            "comment_on_invalid_workflow",
            "linear_contract.comment_on_invalid_workflow",
          ))

          let enabled = enabled_option |> bool_default(defaults.enabled)
          let workflow_label_prefix =
            prefix_option
            |> option.unwrap(defaults.workflow_label_prefix)
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
            |> option.unwrap(defaults.required_states)
          let handoff_state_bindings =
            handoff_bindings_option
            |> option.unwrap(defaults.handoff_state_bindings)
          let enforce_issue_workflow_labels =
            enforce_option
            |> bool_default(defaults.enforce_issue_workflow_labels)
          let invalid_workflow_state_id =
            invalid_state_option
            |> optional_non_empty_string
            |> option.lazy_or(fn() { defaults.invalid_workflow_state_id })
          let comment_on_invalid_workflow =
            comment_option
            |> bool_default(defaults.comment_on_invalid_workflow)
          use handoff_state_bindings <- result.try(validate_handoff_bindings(
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
              Ok(config_types.LinearContractConfig(
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
) -> Result(config_types.LinearCommandConfig, error.ConfigError) {
  let node = get_map(root, "linear_commands")
  let defaults = default_linear_command_config()
  let enabled = get_bool(node, "enabled") |> bool_default(defaults.enabled)
  let prefix = get_string(node, "prefix") |> option.unwrap(defaults.prefix)
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
              Ok(config_types.LinearCommandConfig(
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

fn resolve_routing(
  root: yay.Node,
  config_path: String,
) -> Result(config_types.RoutingConfig, error.ConfigError) {
  let routing = get_map(root, "routing")
  let prefix =
    get_string(routing, "workflow_label_prefix")
    |> option.unwrap("workflow:")
    |> normalize_label
  let require_exactly_one =
    get_bool(routing, "require_exactly_one_workflow_label")
    |> bool_default(False)
  let default_workflow =
    get_non_empty_string(routing, "default_workflow")
    |> option.map(normalize_label)
  use workflows <- result.try(read_routing_workflows(routing, config_path))
  use _ <- result.try(validate_default_workflow(default_workflow, workflows))
  Ok(config_types.RoutingConfig(
    workflow_label_prefix: prefix,
    require_exactly_one_workflow_label: require_exactly_one,
    default_workflow: default_workflow,
    workflows: workflows,
  ))
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
            "routing.default_workflow has invalid workflow id: " <> workflow_id,
          ))
        True ->
          case dict.has_key(workflows, workflow_id) {
            True -> Ok(Nil)
            False ->
              Error(error.InvalidConfig(
                "routing.default_workflow references unknown workflow: "
                <> workflow_id,
              ))
          }
      }
  }
}

fn read_routing_workflows(
  routing: yay.Node,
  config_path: String,
) -> Result(dict.Dict(String, String), error.ConfigError) {
  case get_node(routing, "workflows") {
    None -> Ok(dict.new())
    Some(yay.NodeMap(entries)) ->
      read_routing_workflow_entries(entries, config_path, [])
    Some(_) -> Error(error.InvalidConfig("routing.workflows must be a map"))
  }
}

fn read_routing_workflow_entries(
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
            "routing.workflows has invalid workflow id: " <> key,
          ))
        True -> {
          use path <- result.try(resolve_relative_config_path(
            value,
            config_path,
            "routing.workflows." <> key,
          ))
          read_routing_workflow_entries(rest, config_path, [
            #(workflow_id, path),
            ..acc
          ])
        }
      }
    }
    [#(yay.NodeStr(_), _), ..] ->
      Error(error.InvalidConfig("routing.workflows values must be strings"))
    [#(_, _), ..] ->
      Error(error.InvalidConfig("routing.workflows keys must be strings"))
  }
}

fn resolve_workspace_profiles(
  root: yay.Node,
) -> Result(config_types.WorkspaceHookProfiles, error.ConfigError) {
  let workspace = get_map(root, "workspace")
  let has_legacy_hooks = get_node(workspace, "hooks") != None
  let has_configured_profiles = get_node(workspace, "profiles") != None
  use profiles <- result.try(read_configured_workspace_profiles(workspace))
  use profiles <- result.try(add_legacy_default_profile(
    workspace,
    profiles,
    has_legacy_hooks,
    has_configured_profiles,
  ))
  use default_profile <- result.try(resolve_default_workspace_profile_name(
    workspace,
    profiles,
    has_legacy_hooks,
    has_configured_profiles,
  ))
  Ok(config_types.WorkspaceHookProfiles(
    default_profile: default_profile,
    profiles: profiles,
  ))
}

fn resolve_default_workspace_profile(
  workspace_profiles: config_types.WorkspaceHookProfiles,
) -> Result(config_types.WorkspaceHookProfile, error.ConfigError) {
  case
    dict.get(workspace_profiles.profiles, workspace_profiles.default_profile)
  {
    Ok(profile) -> Ok(profile)
    Error(_) ->
      Error(error.InvalidConfig(
        "workspace.default_profile references unknown profile: "
        <> workspace_profiles.default_profile,
      ))
  }
}

fn read_dag_hooks(
  hooks: yay.Node,
  path: String,
) -> Result(config_types.DagHooksConfig, error.ConfigError) {
  let timeout = get_int(hooks, "timeout_ms") |> int_default(60_000)
  case timeout > 0 {
    False -> Error(error.InvalidConfig(path <> ".timeout_ms must be positive"))
    True ->
      Ok(config_types.DagHooksConfig(
        create: get_non_empty_string(hooks, "create"),
        before_step: get_non_empty_string(hooks, "before_step"),
        after_step: get_non_empty_string(hooks, "after_step"),
        remove: get_non_empty_string(hooks, "remove"),
        timeout_ms: timeout,
      ))
  }
}

fn read_configured_workspace_profiles(
  workspace: yay.Node,
) -> Result(
  dict.Dict(String, config_types.WorkspaceHookProfile),
  error.ConfigError,
) {
  case get_node(workspace, "profiles") {
    None -> Ok(dict.new())
    Some(yay.NodeMap(entries)) -> read_workspace_profile_entries(entries, [])
    Some(_) -> Error(error.InvalidConfig("workspace.profiles must be a map"))
  }
}

fn read_workspace_profile_entries(
  entries: List(#(yay.Node, yay.Node)),
  acc: List(#(String, config_types.WorkspaceHookProfile)),
) -> Result(
  dict.Dict(String, config_types.WorkspaceHookProfile),
  error.ConfigError,
) {
  case entries {
    [] -> Ok(dict.from_list(list.reverse(acc)))
    [#(yay.NodeStr(name), node), ..rest] -> {
      use _ <- result.try(validate_workspace_profile_name(
        name,
        "workspace.profiles." <> name,
      ))
      use profile <- result.try(read_workspace_profile_entry(name, node))
      read_workspace_profile_entries(rest, [#(name, profile), ..acc])
    }
    [#(_, _), ..] ->
      Error(error.InvalidConfig("workspace.profiles keys must be strings"))
  }
}

fn read_workspace_profile_entry(
  name: String,
  node: yay.Node,
) -> Result(config_types.WorkspaceHookProfile, error.ConfigError) {
  let path = "workspace.profiles." <> name
  case node {
    yay.NodeMap(_) ->
      case get_node(node, "hooks") {
        Some(yay.NodeMap(_) as hooks) -> {
          use hooks <- result.try(read_dag_hooks(hooks, path <> ".hooks"))
          Ok(config_types.WorkspaceHookProfile(
            name: name,
            hooks: hooks,
            source: config_types.ConfiguredWorkspaceProfile,
          ))
        }
        Some(_) | None ->
          Error(error.InvalidConfig(path <> ".hooks must be a map"))
      }
    _ -> Error(error.InvalidConfig(path <> " must be a map"))
  }
}

fn add_legacy_default_profile(
  workspace: yay.Node,
  profiles: dict.Dict(String, config_types.WorkspaceHookProfile),
  has_legacy_hooks: Bool,
  has_configured_profiles: Bool,
) -> Result(
  dict.Dict(String, config_types.WorkspaceHookProfile),
  error.ConfigError,
) {
  case has_legacy_hooks || !has_configured_profiles {
    False -> Ok(profiles)
    True ->
      case dict.has_key(profiles, "default") && has_legacy_hooks {
        True ->
          Error(error.InvalidConfig(
            "workspace.profiles.default conflicts with legacy workspace.hooks; move the legacy hooks into profiles.default or rename the profile",
          ))
        False -> {
          let hooks = get_map(workspace, "hooks")
          use hooks <- result.try(read_dag_hooks(hooks, "workspace.hooks"))
          Ok(dict.insert(
            profiles,
            "default",
            config_types.WorkspaceHookProfile(
              name: "default",
              hooks: hooks,
              source: config_types.LegacyWorkspaceHooks,
            ),
          ))
        }
      }
  }
}

fn resolve_default_workspace_profile_name(
  workspace: yay.Node,
  profiles: dict.Dict(String, config_types.WorkspaceHookProfile),
  has_legacy_hooks: Bool,
  has_configured_profiles: Bool,
) -> Result(String, error.ConfigError) {
  case
    get_string_strict(workspace, "default_profile", "workspace.default_profile")
  {
    Error(err) -> Error(err)
    Ok(Some(raw_default)) -> {
      let default_profile = string.trim(raw_default)
      use _ <- result.try(validate_workspace_profile_name(
        default_profile,
        "workspace.default_profile",
      ))
      case dict.has_key(profiles, default_profile) {
        True -> Ok(default_profile)
        False ->
          Error(error.InvalidConfig(
            "workspace.default_profile references unknown profile: "
            <> default_profile,
          ))
      }
    }
    Ok(None) ->
      case has_legacy_hooks || !has_configured_profiles {
        True -> Ok("default")
        False ->
          Error(error.InvalidConfig(
            "workspace.default_profile is required when workspace.profiles is set without workspace.hooks",
          ))
      }
  }
}

fn validate_workspace_profile_name(
  value: String,
  path: String,
) -> Result(Nil, error.ConfigError) {
  case valid_workflow_name(value) {
    True -> Ok(Nil)
    False ->
      Error(error.InvalidConfig(path <> " has invalid profile name: " <> value))
  }
}

fn resolve_artifact_limits(
  root: yay.Node,
) -> Result(config_types.ArtifactLimits, error.ConfigError) {
  let limits = get_map(root, "artifact_limits")
  let command_stream_max_chars =
    get_int(limits, "command_stream_max_chars") |> int_default(20_000)
  let template_field_max_chars =
    get_int(limits, "template_field_max_chars") |> int_default(8000)
  let workflow_summary_max_chars =
    get_int(limits, "workflow_summary_max_chars") |> int_default(20_000)
  case
    command_stream_max_chars <= 0
    || template_field_max_chars <= 0
    || workflow_summary_max_chars <= 0
  {
    True ->
      Error(error.InvalidConfig("artifact_limits values must be positive"))
    False ->
      Ok(config_types.ArtifactLimits(
        command_stream_max_chars: command_stream_max_chars,
        template_field_max_chars: template_field_max_chars,
        workflow_summary_max_chars: workflow_summary_max_chars,
      ))
  }
}

fn resolve_workflow_model_settings(
  root: yay.Node,
) -> Result(model_config.Settings, error.ConfigError) {
  let pi = get_map(root, "pi")
  model_config.read_settings(
    pi,
    model_config.SettingsPaths(
      provider_path: "pi.provider",
      provider_model_path: "pi.model",
      model_path: "pi.model",
      thinking_path: "pi.thinking",
    ),
    fn(_code, message) { error.InvalidConfig(message) },
  )
}

fn resolve_scheduled_jobs(
  root: yay.Node,
  routing: config_types.RoutingConfig,
) -> Result(List(config_types.ScheduledJobConfig), error.ConfigError) {
  case get_node(root, "scheduled_jobs") {
    None -> Ok([])
    Some(yay.NodeSeq(values)) ->
      read_scheduled_job_values(values, routing, [], [])
    Some(_) -> Error(error.InvalidConfig("scheduled_jobs must be a list"))
  }
}

fn read_scheduled_job_values(
  values: List(yay.Node),
  routing: config_types.RoutingConfig,
  seen_ids: List(String),
  acc: List(config_types.ScheduledJobConfig),
) -> Result(List(config_types.ScheduledJobConfig), error.ConfigError) {
  case values {
    [] -> Ok(list.reverse(acc))
    [yay.NodeMap(_) as node, ..rest] -> {
      use job <- result.try(resolve_scheduled_job(node, routing))
      case list.contains(seen_ids, job.id) {
        True ->
          Error(error.InvalidConfig(
            "scheduled_jobs has duplicate job id: " <> job.id,
          ))
        False ->
          read_scheduled_job_values(rest, routing, [job.id, ..seen_ids], [
            job,
            ..acc
          ])
      }
    }
    [_, ..] -> Error(error.InvalidConfig("scheduled_jobs entries must be maps"))
  }
}

fn resolve_scheduled_job(
  node: yay.Node,
  routing: config_types.RoutingConfig,
) -> Result(config_types.ScheduledJobConfig, error.ConfigError) {
  use _ <- result.try(reject_schedule_payload_fields(node))
  use id_raw <- result.try(get_required_string(
    node,
    "id",
    error.InvalidConfig("scheduled_jobs entries require id"),
  ))
  let id = normalize_label(id_raw)
  use _ <- result.try(validate_scheduled_id(id, "scheduled_jobs.id"))
  use workflow_raw <- result.try(get_required_string(
    node,
    "workflow",
    error.InvalidConfig("scheduled_jobs." <> id <> " requires workflow"),
  ))
  let workflow = normalize_label(workflow_raw)
  use _ <- result.try(validate_scheduled_id(
    workflow,
    "scheduled_jobs." <> id <> ".workflow",
  ))
  use enabled_option <- result.try(get_bool_strict(
    node,
    "enabled",
    "scheduled_jobs." <> id <> ".enabled",
  ))
  let enabled = enabled_option |> bool_default(True)
  use every_ms <- result.try(resolve_scheduled_every(node, id, enabled))
  use overlap <- result.try(resolve_scheduled_overlap(node, id))
  use catch_up <- result.try(resolve_scheduled_catch_up(node, id))
  use on_failure <- result.try(resolve_scheduled_failure(node, id))
  case enabled && !dict.has_key(routing.workflows, workflow) {
    True ->
      Error(error.InvalidConfig(
        "scheduled_jobs."
        <> id
        <> ".workflow references unknown workflow: "
        <> workflow,
      ))
    False ->
      Ok(config_types.ScheduledJobConfig(
        id: id,
        workflow: workflow,
        enabled: enabled,
        every_ms: every_ms,
        overlap: overlap,
        catch_up: catch_up,
        on_failure: on_failure,
      ))
  }
}

fn validate_scheduled_id(
  value: String,
  path: String,
) -> Result(Nil, error.ConfigError) {
  case valid_workflow_name(value) {
    True -> Ok(Nil)
    False -> Error(error.InvalidConfig(path <> " has invalid id: " <> value))
  }
}

fn resolve_scheduled_every(
  node: yay.Node,
  id: String,
  enabled: Bool,
) -> Result(Int, error.ConfigError) {
  case get_string_strict(node, "every", "scheduled_jobs." <> id <> ".every") {
    Error(err) -> Error(err)
    Ok(None) ->
      case enabled {
        True ->
          Error(error.InvalidConfig(
            "scheduled_jobs." <> id <> ".every is required when enabled",
          ))
        False -> Ok(0)
      }
    Ok(Some(value)) -> {
      use every_ms <- result.try(
        schedule_core.parse_every(value)
        |> result.map_error(fn(message) {
          error.InvalidConfig("scheduled_jobs." <> id <> ".every: " <> message)
        }),
      )
      case enabled && every_ms < 1000 {
        True ->
          Error(error.InvalidConfig(
            "scheduled_jobs."
            <> id
            <> ".every must be at least 1000ms when enabled",
          ))
        False -> Ok(every_ms)
      }
    }
  }
}

fn resolve_scheduled_overlap(
  node: yay.Node,
  id: String,
) -> Result(config_types.ScheduledOverlap, error.ConfigError) {
  case
    get_string_strict(node, "overlap", "scheduled_jobs." <> id <> ".overlap")
  {
    Error(err) -> Error(err)
    Ok(None) -> Ok(config_types.SkipOverlap)
    Ok(Some(value)) ->
      case value |> string.trim |> string.lowercase {
        "skip" -> Ok(config_types.SkipOverlap)
        other ->
          Error(error.InvalidScheduledJobOverlap(
            "scheduled_jobs."
            <> id
            <> ".overlap unsupported value "
            <> other
            <> "; the MVP accepts only skip",
          ))
      }
  }
}

fn resolve_scheduled_catch_up(
  node: yay.Node,
  id: String,
) -> Result(Bool, error.ConfigError) {
  use catch_up <- result.try(get_bool_strict(
    node,
    "catch_up",
    "scheduled_jobs." <> id <> ".catch_up",
  ))
  case catch_up |> bool_default(False) {
    False -> Ok(False)
    True ->
      Error(error.ScheduledJobCatchUpUnsupported(
        "scheduled_jobs." <> id <> ".catch_up=true is not supported in the MVP",
      ))
  }
}

fn resolve_scheduled_failure(
  node: yay.Node,
  id: String,
) -> Result(config_types.ScheduledFailureConfig, error.ConfigError) {
  case get_node(node, "on_failure") {
    None -> Ok(default_scheduled_failure_config())
    Some(yay.NodeMap(_)) ->
      resolve_scheduled_on_failure_map(get_map(node, "on_failure"), id)
    Some(_) ->
      Error(error.InvalidConfig(
        "scheduled_jobs." <> id <> ".on_failure must be a map",
      ))
  }
}

fn resolve_scheduled_on_failure_map(
  node: yay.Node,
  id: String,
) -> Result(config_types.ScheduledFailureConfig, error.ConfigError) {
  case get_node(node, "linear") {
    None -> Ok(default_scheduled_failure_config())
    Some(yay.NodeMap(_)) -> {
      use linear <- result.try(resolve_scheduled_linear_failure(
        get_map(node, "linear"),
        id,
      ))
      Ok(config_types.ScheduledFailureConfig(linear: linear))
    }
    Some(_) ->
      Error(error.InvalidConfig(
        "scheduled_jobs." <> id <> ".on_failure.linear must be a map",
      ))
  }
}

fn resolve_scheduled_linear_failure(
  node: yay.Node,
  id: String,
) -> Result(config_types.ScheduledLinearFailureConfig, error.ConfigError) {
  let path = "scheduled_jobs." <> id <> ".on_failure.linear"
  use enabled_option <- result.try(get_bool_strict(
    node,
    "enabled",
    path <> ".enabled",
  ))
  let enabled = enabled_option |> bool_default(False)
  use state_option <- result.try(get_optional_string_strict(
    node,
    "state",
    path <> ".state",
  ))
  let state = state_option |> optional_non_empty_string
  use labels <- result.try(get_optional_string_list_strict(
    node,
    "labels",
    path <> ".labels",
  ))
  use dedupe <- result.try(resolve_scheduled_failure_dedupe(node, path))
  case enabled, state {
    True, None ->
      Error(error.InvalidConfig(
        path <> ".state is required when Linear failure reporting is enabled",
      ))
    _, _ ->
      Ok(config_types.ScheduledLinearFailureConfig(
        enabled: enabled,
        state: state,
        labels: labels |> normalize_string_list,
        dedupe: dedupe,
      ))
  }
}

fn resolve_scheduled_failure_dedupe(
  node: yay.Node,
  path: String,
) -> Result(config_types.ScheduledFailureDedupe, error.ConfigError) {
  case get_string_strict(node, "dedupe", path <> ".dedupe") {
    Error(err) -> Error(err)
    Ok(None) -> Ok(config_types.OpenIssuePerJob)
    Ok(Some(value)) ->
      case value |> string.trim |> string.lowercase {
        "open_issue_per_job" -> Ok(config_types.OpenIssuePerJob)
        other ->
          Error(error.InvalidConfig(
            path
            <> ".dedupe unsupported value "
            <> other
            <> "; the MVP accepts only open_issue_per_job",
          ))
      }
  }
}

fn reject_schedule_payload_fields(
  node: yay.Node,
) -> Result(Nil, error.ConfigError) {
  case node {
    yay.NodeMap(entries) -> reject_schedule_payload_entries(entries)
    _ -> Ok(Nil)
  }
}

fn reject_schedule_payload_entries(
  entries: List(#(yay.Node, yay.Node)),
) -> Result(Nil, error.ConfigError) {
  case entries {
    [] -> Ok(Nil)
    [#(yay.NodeStr(key), _), ..rest] ->
      case
        list.contains(["input", "inputs", "vars", "variables", "payload"], key)
      {
        True ->
          Error(error.ScheduledJobUnsupportedInputs(
            "scheduled_jobs."
            <> key
            <> " is intentionally deferred; put job-specific details in workflow YAML, prompt files, scripts, environment, or repository config",
          ))
        False -> reject_schedule_payload_entries(rest)
      }
    [_, ..rest] -> reject_schedule_payload_entries(rest)
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

fn get_optional_string_list_strict(
  node: yay.Node,
  key: String,
  path: String,
) -> Result(List(String), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok([])
    Some(yay.NodeSeq(values)) -> read_string_values(values, path, [])
    Some(_) -> Error(error.InvalidConfig(path <> " must be a string list"))
  }
}

fn resolve_orchestrator_linear_contract(
  root: yay.Node,
  contract: config_types.LinearContractConfig,
  routing: config_types.RoutingConfig,
) -> Result(config_types.LinearContractConfig, error.ConfigError) {
  let workflow_names =
    dict.keys(routing.workflows)
    |> normalize_label_list
    |> list.sort(by: string.compare)
  let has_labels = linear_contract_field_present(root, "workflow_labels")
  let has_prefix = linear_contract_field_present(root, "workflow_label_prefix")
  let contract_prefix = case has_prefix {
    True -> contract.workflow_label_prefix
    False -> routing.workflow_label_prefix
  }
  case
    has_prefix
    && contract.workflow_label_prefix != routing.workflow_label_prefix
  {
    True ->
      Error(error.InvalidConfig(
        "linear_contract.workflow_label_prefix must match routing.workflow_label_prefix",
      ))
    False -> {
      let contract_names =
        contract.workflow_labels
        |> normalize_label_list
        |> list.sort(by: string.compare)
      case routing.require_exactly_one_workflow_label, has_labels {
        True, False ->
          Ok(
            config_types.LinearContractConfig(
              ..contract,
              workflow_label_prefix: contract_prefix,
              workflow_labels: workflow_names,
            ),
          )
        True, True ->
          case contract_names == workflow_names {
            True ->
              Ok(
                config_types.LinearContractConfig(
                  ..contract,
                  workflow_label_prefix: contract_prefix,
                  workflow_labels: workflow_names,
                ),
              )
            False ->
              Error(error.InvalidConfig(
                "linear_contract.workflow_labels must match routing.workflows when routing requires exactly one workflow label",
              ))
          }
        _, _ ->
          Ok(
            config_types.LinearContractConfig(
              ..contract,
              workflow_label_prefix: contract_prefix,
            ),
          )
      }
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
      case string.starts_with(trimmed, "/") || has_parent_segment(trimmed) {
        True ->
          Error(error.InvalidConfig(
            field <> " must be a relative path without ..",
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
  |> result.map(fn(_) { bindings })
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
) -> Result(config_types.UiRequestPolicy, error.ConfigError) {
  case value {
    Some(value) ->
      case string.lowercase(string.trim(value)) {
        "cancel" -> Ok(config_types.Cancel)
        "fail" -> Ok(config_types.Fail)
        "ignore" -> Ok(config_types.Ignore)
        "operator" -> Ok(config_types.Operator)
        other ->
          Error(error.InvalidConfig("invalid pi.ui_request_policy: " <> other))
      }
    None -> Ok(config_types.Cancel)
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

fn get_string_map_strict(
  node: yay.Node,
  key: String,
  path: String,
) -> Result(List(#(String, String)), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok([])
    Some(yay.NodeMap(entries)) -> read_string_map_entries(entries, path, [])
    Some(_) -> Error(error.InvalidConfig(path <> " must be a string map"))
  }
}

fn read_string_map_entries(
  entries: List(#(yay.Node, yay.Node)),
  path: String,
  acc: List(#(String, String)),
) -> Result(List(#(String, String)), error.ConfigError) {
  case entries {
    [] -> Ok(list.reverse(acc))
    [#(yay.NodeStr(key), yay.NodeStr(value)), ..rest] ->
      read_string_map_entries(rest, path, [#(key, value), ..acc])
    [#(yay.NodeStr(_), _), ..] ->
      Error(error.InvalidConfig(path <> " values must be strings"))
    [#(_, _), ..] -> Error(error.InvalidConfig(path <> " keys must be strings"))
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
      use strings <- result.try(read_contract_string_list(values, path, []))
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
      use pairs <- result.try(
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

fn get_positive_int_map(
  node: yay.Node,
  key: String,
) -> dict.Dict(issue_state.IssueStateKey, Int) {
  case node {
    yay.NodeMap(pairs) ->
      case list.key_find(pairs, yay.NodeStr(key)) {
        Ok(yay.NodeMap(entries)) ->
          entries
          |> list.filter_map(fn(entry) {
            case entry {
              #(yay.NodeStr(k), yay.NodeInt(v)) if v > 0 ->
                Ok(#(issue_state.key_from_string(k), v))
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
          |> result.unwrap(path)
        Error(_) -> path
      }
    False ->
      case string.starts_with(path, "/") {
        True -> absname(path) |> result.unwrap(path)
        False -> {
          let dir = dirname(workflow_path) |> result.unwrap(".")
          absname(dir <> "/" <> path) |> result.unwrap(path)
        }
      }
  }
}

fn default_workspace_root(workflow_path: String) -> String {
  let tmp = tmpdir() |> result.unwrap("/tmp")
  let _ = workflow_path
  absname(tmp <> "/scherzo_workspaces")
  |> result.unwrap(tmp <> "/scherzo_workspaces")
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
  option.unwrap(value, default)
}

fn bool_default(value: Option(Bool), default: Bool) -> Bool {
  option.unwrap(value, default)
}

fn list_default(value: Option(List(a)), default: List(a)) -> List(a) {
  option.unwrap(value, default)
}

fn config_error_message(err: error.ConfigError) -> String {
  error.config_message(err)
}

@external(erlang, "scherzo_config_ffi", "home")
fn home() -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "tmpdir")
fn tmpdir() -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "dirname")
fn dirname(path: String) -> Result(String, Nil)

@external(erlang, "scherzo_config_ffi", "absname")
fn absname(path: String) -> Result(String, Nil)
