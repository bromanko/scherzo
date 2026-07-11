import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{Gt, Lt}
import gleam/result
import gleam/string
import gleam/uri
import scherzo/artifact_publication_config
import scherzo/config/duration_config
import scherzo/config/tracker_config
import scherzo/config/types as config_types
import scherzo/control/remote/config_validation as remote_config_validation
import scherzo/error
import scherzo/model_config
import scherzo/tracker/state as issue_state
import scherzo/workflow_completion_policy
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

type AgentConcurrency {
  AgentConcurrency(
    default: Int,
    by_state: dict.Dict(issue_state.IssueStateKey, Int),
  )
}

type RuntimeSessions {
  EphemeralSessions
  PersistentSessions
}

pub fn config_warning_message(warning: config_types.ConfigWarning) -> String {
  config_types.config_warning_message(warning)
}

pub fn default_tracker_config() -> config_types.TrackerConfig {
  tracker_config.default()
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

pub fn default_control_config() -> config_types.ControlConfig {
  config_types.ControlConfig(
    command_timeout_ms: duration_config.default_control_command_timeout_ms,
  )
}

pub fn default_ledger_compaction_config() -> config_types.LedgerCompactionConfig {
  config_types.LedgerCompactionConfig(
    enabled: True,
    max_current_records: 10_000,
    max_current_bytes: 8 * 1024 * 1024,
    min_interval_ms: 300_000,
  )
}

pub fn default_effective_config(
  tracker: config_types.TrackerConfig,
  workspace: config_types.WorkspaceConfig,
) -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: tracker,
    polling: default_polling_config(),
    workspace: workspace,
    control: default_control_config(),
    ledger_compaction: default_ledger_compaction_config(),
    hooks: default_hooks_config(),
    agent: default_agent_config(),
    pi: default_pi_config(),
    handoff: default_handoff_config(),
    linear_contract: default_linear_contract_config(),
    linear_commands: default_linear_command_config(),
    ui_server: default_ui_server_config(),
  )
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
    max_concurrent_agents: 1,
    max_turns: 20,
    max_sessions_per_issue: 3,
    context_recovery_max_attempts: 1,
    context_recovery_prompt_char_limit: 40_000,
    max_concurrent_agents_by_state: dict.new(),
  )
}

pub fn default_recovery_prompt() -> String {
  "You are being resumed by Scherzo after an interrupted workflow agent step.\nContinue from the existing pi session context that was reopened for this step.\nDo not restart from scratch and do not assume the original prompt has been resent.\nWork in the current directory, which is the recorded workspace for this step.\nIf the prior session context shows the step was already completed, summarize the completed work and stop.\nOtherwise, inspect the current workspace as needed, finish the same step, and provide a concise final response for Scherzo."
}

pub fn default_pi_config() -> config_types.PiConfig {
  config_types.PiConfig(
    command: "pi --mode rpc --no-session --rpc-message-updates off",
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
    completion_states: None,
  )
}

pub fn default_ui_server_config() -> config_types.UiServerConfig {
  config_types.UiServerDisabled(
    endpoint: None,
    credential_ref: None,
    daemon_label: None,
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
    invalid_workflow_state_target: None,
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
    task: config_types.ScheduledTaskFailureConfig(
      enabled: False,
      state: None,
      labels: [],
      dedupe: config_types.OpenTaskPerSchedule,
    ),
  )
}

fn config_migration_hint(
  old_path: String,
  replacement: String,
) -> error.ConfigError {
  error.InvalidConfig(
    old_path
    <> " was removed. Use "
    <> replacement
    <> ". See docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md.",
  )
}

pub fn resolve_with_env(
  root: yay.Node,
  config_path: String,
  env: Env,
) -> Result(config_types.EffectiveConfig, error.ConfigError) {
  use report <- result.try(resolve_with_env_report(root, config_path, env))
  Ok(report.config)
}

pub fn resolve_with_env_report(
  root: yay.Node,
  config_path: String,
  env: Env,
) -> Result(config_types.ResolveReport, error.ConfigError) {
  resolve_root_report(root, config_path, env)
}

pub fn resolve_root(
  root: yay.Node,
  config_path: String,
  env: Env,
) -> Result(config_types.EffectiveConfig, error.ConfigError) {
  use report <- result.try(resolve_root_report(root, config_path, env))
  Ok(report.config)
}

pub fn resolve_root_report(
  root: yay.Node,
  config_path: String,
  env: Env,
) -> Result(config_types.ResolveReport, error.ConfigError) {
  use _ <- result.try(tracker_config.reject_root_removed_keys(root))
  use tracker_result <- result.try(tracker_config.resolve(root, env))
  let #(tracker, tracker_warnings) = tracker_result
  use polling <- result.try(resolve_polling(root))
  use workspace <- result.try(resolve_workspace(root, config_path, env))
  use control <- result.try(resolve_control(root))
  use ledger_compaction <- result.try(resolve_ledger_compaction(root))
  use hooks <- result.try(resolve_hooks(root))
  use agent <- result.try(resolve_agent(root))
  use pi <- result.try(resolve_pi(root))
  use linear_contract <- result.try(resolve_linear_contract(root))
  use handoff <- result.try(resolve_handoff(root))
  use linear_commands <- result.try(resolve_linear_commands(root))
  use ui_server <- result.try(resolve_ui_server(root, env))
  Ok(config_types.ResolveReport(
    config: config_types.EffectiveConfig(
      tracker:,
      polling:,
      workspace:,
      control:,
      ledger_compaction:,
      hooks:,
      agent:,
      pi:,
      handoff:,
      linear_contract:,
      linear_commands:,
      ui_server:,
    ),
    warnings: tracker_warnings,
  ))
}

pub fn resolve_orchestrator_root(
  root: yay.Node,
  config_path: String,
  env: Env,
) -> Result(config_types.OrchestratorConfig, error.ConfigError) {
  use effective <- result.try(resolve_root(root, config_path, env))
  use routing <- result.try(tracker_config.resolve_root_routing(
    root,
    config_path,
  ))
  use workspace_profiles <- result.try(resolve_workspace_profiles(root))
  use _default_workspace_profile <- result.try(
    resolve_default_workspace_profile(workspace_profiles),
  )
  let dag_hooks = config_types.empty_dag_hooks()
  use artifact_limits <- result.try(resolve_artifact_limits(root))
  use artifact_repositories <- result.try(resolve_artifact_repositories(root))
  use model_settings <- result.try(resolve_workflow_model_settings(root))
  use scheduled_jobs <- result.try(resolve_scheduled_jobs(root, routing))
  use linear_contract <- result.try(
    tracker_config.resolve_root_orchestrator_linear_contract(
      root,
      effective,
      routing,
      scheduled_jobs,
    ),
  )
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
    artifact_repositories: artifact_repositories,
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

fn resolve_polling(
  root: yay.Node,
) -> Result(config_types.PollingConfig, error.ConfigError) {
  use interval <- result.try(duration_config.polling_interval_ms(root))
  Ok(config_types.PollingConfig(interval_ms: interval))
}

fn resolve_workspace(
  root: yay.Node,
  workflow_path: String,
  env: Env,
) -> Result(config_types.WorkspaceConfig, error.ConfigError) {
  let workspace = get_map(root, "workspace")
  use _ <- result.try(tracker_config.resolve_workspace_drivers(workspace))
  let raw = get_string(workspace, "root")
  let root =
    raw
    |> resolve_optional_env(env)
    |> option.unwrap(default_workspace_root(workflow_path))
  Ok(config_types.WorkspaceConfig(root: resolve_path(root, workflow_path)))
}

fn resolve_control(
  root: yay.Node,
) -> Result(config_types.ControlConfig, error.ConfigError) {
  let defaults = default_control_config()
  use control <- result.try(get_map_strict_or_empty(root, "control", "control"))
  use _ <- result.try(
    reject_unknown_map_keys(control, "control", [
      "command_timeout",
    ]),
  )
  use command_timeout_ms <- result.try(
    duration_config.control_command_timeout_ms(
      root,
      defaults.command_timeout_ms,
    ),
  )
  Ok(config_types.ControlConfig(command_timeout_ms: command_timeout_ms))
}

fn resolve_ledger_compaction(
  root: yay.Node,
) -> Result(config_types.LedgerCompactionConfig, error.ConfigError) {
  let defaults = default_ledger_compaction_config()
  use state_ledger <- result.try(get_map_strict_or_empty(
    root,
    "state_ledger",
    "state_ledger",
  ))
  use _ <- result.try(
    reject_unknown_map_keys(state_ledger, "state_ledger", [
      "auto_compaction",
    ]),
  )
  use auto_compaction <- result.try(get_map_strict_or_empty(
    state_ledger,
    "auto_compaction",
    "state_ledger.auto_compaction",
  ))
  use _ <- result.try(
    reject_unknown_map_keys(auto_compaction, "state_ledger.auto_compaction", [
      "enabled",
      "max_current_records",
      "max_current_bytes",
      "min_interval",
    ]),
  )
  use enabled <- result.try(get_bool_strict(
    auto_compaction,
    "enabled",
    "state_ledger.auto_compaction.enabled",
  ))
  use max_current_records <- result.try(get_int_strict(
    auto_compaction,
    "max_current_records",
    "state_ledger.auto_compaction.max_current_records",
  ))
  use max_current_bytes <- result.try(get_int_strict(
    auto_compaction,
    "max_current_bytes",
    "state_ledger.auto_compaction.max_current_bytes",
  ))
  use min_interval_ms <- result.try(
    duration_config.ledger_compaction_min_interval_ms(
      root,
      defaults.min_interval_ms,
    ),
  )
  let max_current_records =
    max_current_records |> int_default(defaults.max_current_records)
  let max_current_bytes =
    max_current_bytes |> int_default(defaults.max_current_bytes)
  case max_current_records <= 0 {
    True ->
      Error(error.InvalidConfig(
        "state_ledger.auto_compaction.max_current_records must be positive",
      ))
    False ->
      case max_current_bytes <= 0 {
        True ->
          Error(error.InvalidConfig(
            "state_ledger.auto_compaction.max_current_bytes must be positive",
          ))
        False ->
          case min_interval_ms <= 0 {
            True ->
              Error(error.InvalidConfig(
                "state_ledger.auto_compaction.min_interval must be positive",
              ))
            False ->
              Ok(config_types.LedgerCompactionConfig(
                enabled: enabled |> bool_default(defaults.enabled),
                max_current_records: max_current_records,
                max_current_bytes: max_current_bytes,
                min_interval_ms: min_interval_ms,
              ))
          }
      }
  }
}

fn resolve_hooks(
  root: yay.Node,
) -> Result(config_types.HooksConfig, error.ConfigError) {
  let hooks = get_map(root, "hooks")
  use timeout <- result.try(duration_config.hooks_timeout_ms(root))
  Ok(config_types.HooksConfig(
    after_create: get_non_empty_string(hooks, "after_create"),
    before_run: get_non_empty_string(hooks, "before_run"),
    after_run: get_non_empty_string(hooks, "after_run"),
    before_remove: get_non_empty_string(hooks, "before_remove"),
    timeout_ms: timeout,
  ))
}

fn resolve_agent(
  root: yay.Node,
) -> Result(config_types.AgentConfig, error.ConfigError) {
  use agents <- result.try(get_map_strict_or_empty(root, "agents", "agents"))
  use recovery <- result.try(get_map_strict_or_empty(
    agents,
    "recovery",
    "agents.recovery",
  ))
  use concurrency <- result.try(resolve_agent_concurrency(agents))
  let AgentConcurrency(default: max_concurrent_agents, by_state:) = concurrency
  let max_turns = get_int(agents, "max_turns") |> int_default(20)
  let max_sessions_per_issue =
    get_int(agents, "sessions_per_task") |> int_default(3)
  let context_recovery_max_attempts =
    get_int(recovery, "attempts") |> int_default(1)
  let context_recovery_prompt_char_limit =
    get_int(recovery, "prompt_char_limit") |> int_default(40_000)

  case max_concurrent_agents < 0 || context_recovery_max_attempts < 0 {
    True ->
      Error(error.InvalidConfig(
        "agents.concurrency and agents.recovery.attempts must be zero or positive",
      ))
    False ->
      case
        max_turns <= 0
        || max_sessions_per_issue <= 0
        || context_recovery_prompt_char_limit <= 0
      {
        True -> Error(error.InvalidConfig("agents limits must be positive"))
        False ->
          Ok(config_types.AgentConfig(
            max_concurrent_agents: max_concurrent_agents,
            max_turns: max_turns,
            max_sessions_per_issue: max_sessions_per_issue,
            context_recovery_max_attempts: context_recovery_max_attempts,
            context_recovery_prompt_char_limit: context_recovery_prompt_char_limit,
            max_concurrent_agents_by_state: by_state,
          ))
      }
  }
}

fn resolve_agent_concurrency(
  agents: yay.Node,
) -> Result(AgentConcurrency, error.ConfigError) {
  case get_node(agents, "concurrency") {
    None -> Ok(AgentConcurrency(default: 1, by_state: dict.new()))
    Some(yay.NodeInt(value)) ->
      Ok(AgentConcurrency(default: value, by_state: dict.new()))
    Some(yay.NodeMap(_) as concurrency) ->
      Ok(AgentConcurrency(
        default: get_int(concurrency, "default") |> int_default(1),
        by_state: get_positive_int_map(concurrency, "by_state"),
      ))
    Some(_) ->
      Error(error.InvalidConfig(
        "agents.concurrency must be an integer or a map with default and by_state",
      ))
  }
}

fn resolve_pi(
  root: yay.Node,
) -> Result(config_types.PiConfig, error.ConfigError) {
  use agents <- result.try(get_map_strict_or_empty(root, "agents", "agents"))
  case get_node(agents, "runtime") {
    None -> Ok(default_pi_config())
    Some(yay.NodeMap(_) as runtime) -> resolve_pi_runtime(root, runtime)
    Some(_) -> Error(error.InvalidConfig("agents.runtime must be a map"))
  }
}

fn resolve_pi_runtime(
  root: yay.Node,
  runtime: yay.Node,
) -> Result(config_types.PiConfig, error.ConfigError) {
  use _ <- result.try(resolve_runtime_type(runtime))
  use runtime_pi <- result.try(get_map_strict_or_empty(
    runtime,
    "pi",
    "agents.runtime.pi",
  ))
  use sessions <- result.try(resolve_runtime_sessions(runtime))
  use executable <- result.try(resolve_runtime_pi_executable(runtime_pi))
  use user_args <- result.try(resolve_runtime_pi_args(runtime_pi))
  use env <- result.try(resolve_runtime_pi_env(runtime_pi))
  use turn_timeout_ms <- result.try(duration_config.pi_turn_timeout_ms(root))
  use read_timeout_ms <- result.try(duration_config.pi_read_timeout_ms(root))
  use stall_timeout_ms <- result.try(duration_config.pi_stall_timeout_ms(root))
  use ui_request_timeout_ms <- result.try(
    duration_config.pi_ui_request_timeout_ms(root),
  )
  case
    turn_timeout_ms <= 0
    || read_timeout_ms <= 0
    || stall_timeout_ms < 0
    || ui_request_timeout_ms <= 0
  {
    True -> Error(error.InvalidConfig("invalid agents.runtime config"))
    False -> {
      let args = runtime_pi_args(user_args, sessions)
      let session_persistence =
        config_types.PiSessionPersistenceConfig(
          enabled: runtime_sessions_persistent(sessions),
          recovery_prompt: default_recovery_prompt(),
        )
      use ui_request_policy <- result.try(ui_policy_at(
        get_string(runtime, "ui_requests"),
        "agents.runtime.ui_requests",
      ))
      Ok(config_types.PiConfig(
        command: runtime_pi_shell_command(executable, args),
        turn_timeout_ms: turn_timeout_ms,
        read_timeout_ms: read_timeout_ms,
        stall_timeout_ms: stall_timeout_ms,
        auto_retry: get_bool(runtime, "auto_retry") |> bool_default(True),
        ui_request_policy: ui_request_policy,
        ui_request_timeout_ms: ui_request_timeout_ms,
        compatibility_probe: get_bool(runtime, "compatibility_check")
          |> bool_default(True),
        rate_limit_payload: None,
        argv_command: Some(config_types.PiArgvCommand(
          executable: executable,
          args: args,
          env: env,
        )),
        session_persistence: session_persistence,
      ))
    }
  }
}

fn resolve_runtime_type(runtime: yay.Node) -> Result(Nil, error.ConfigError) {
  use type_option <- result.try(get_string_strict(
    runtime,
    "type",
    "agents.runtime.type",
  ))
  case type_option {
    None ->
      Error(error.InvalidConfig(
        "agents.runtime.type is required when agents.runtime is present",
      ))
    Some(value) ->
      case value |> string.trim |> string.lowercase {
        "pi" -> Ok(Nil)
        other ->
          Error(error.InvalidConfig(
            "agents.runtime.type must be pi; unsupported runtime: " <> other,
          ))
      }
  }
}

fn resolve_runtime_sessions(
  runtime: yay.Node,
) -> Result(RuntimeSessions, error.ConfigError) {
  use sessions_option <- result.try(get_string_strict(
    runtime,
    "sessions",
    "agents.runtime.sessions",
  ))
  let sessions = sessions_option |> option.unwrap("ephemeral")
  case sessions |> string.trim |> string.lowercase {
    "ephemeral" -> Ok(EphemeralSessions)
    "persistent" -> Ok(PersistentSessions)
    other ->
      Error(error.InvalidConfig(
        "agents.runtime.sessions must be ephemeral or persistent: " <> other,
      ))
  }
}

fn runtime_sessions_persistent(sessions: RuntimeSessions) -> Bool {
  case sessions {
    EphemeralSessions -> False
    PersistentSessions -> True
  }
}

fn resolve_runtime_pi_executable(
  runtime_pi: yay.Node,
) -> Result(String, error.ConfigError) {
  use executable_option <- result.try(get_string_strict(
    runtime_pi,
    "executable",
    "agents.runtime.pi.executable",
  ))
  let executable = executable_option |> option.unwrap("pi") |> string.trim
  case executable == "" {
    True ->
      Error(error.InvalidConfig(
        "agents.runtime.pi.executable must be non-empty",
      ))
    False -> Ok(executable)
  }
}

fn resolve_runtime_pi_args(
  runtime_pi: yay.Node,
) -> Result(List(String), error.ConfigError) {
  use args <- result.try(get_optional_string_list_strict(
    runtime_pi,
    "args",
    "agents.runtime.pi.args",
  ))
  case list.find(args, forbidden_runtime_arg) {
    Ok(arg) ->
      Error(error.InvalidConfig(
        "agents.runtime.pi.args must not contain Scherzo-owned pi flags: "
        <> arg
        <> ". Remove --session, --no-session, --mode, and --rpc-message-updates; Scherzo adds protocol and session flags automatically.",
      ))
    Error(Nil) -> Ok(args)
  }
}

fn forbidden_runtime_arg(arg: String) -> Bool {
  arg == "--session"
  || string.starts_with(arg, "--session=")
  || arg == "--no-session"
  || arg == "--mode"
  || string.starts_with(arg, "--mode=")
  || arg == "--rpc-message-updates"
  || string.starts_with(arg, "--rpc-message-updates=")
}

fn resolve_runtime_pi_env(
  runtime_pi: yay.Node,
) -> Result(List(#(String, String)), error.ConfigError) {
  use env <- result.try(get_string_map_strict(
    runtime_pi,
    "env",
    "agents.runtime.pi.env",
  ))
  case
    list.find(env, fn(entry) {
      let #(key, _) = entry
      !valid_env_key(key)
    })
  {
    Ok(#(key, _)) ->
      Error(error.InvalidConfig(
        "agents.runtime.pi.env keys must be valid environment variable names: "
        <> key,
      ))
    Error(Nil) -> Ok(env)
  }
}

fn valid_env_key(key: String) -> Bool {
  case string.to_graphemes(key) {
    [] -> False
    [first, ..rest] -> is_env_key_start(first) && all(rest, is_env_key_char)
  }
}

fn is_env_key_start(ch: String) -> Bool {
  is_between(ch, "a", "z") || is_between(ch, "A", "Z") || ch == "_"
}

fn is_env_key_char(ch: String) -> Bool {
  is_env_key_start(ch) || is_between(ch, "0", "9")
}

fn runtime_pi_args(
  user_args: List(String),
  sessions: RuntimeSessions,
) -> List(String) {
  list.append(user_args, runtime_protocol_args(sessions))
}

fn runtime_protocol_args(sessions: RuntimeSessions) -> List(String) {
  let session_args = case sessions {
    EphemeralSessions -> ["--no-session"]
    PersistentSessions -> []
  }
  list.append(
    ["--mode", "rpc"],
    list.append(session_args, ["--rpc-message-updates", "off"]),
  )
}

fn runtime_pi_shell_command(executable: String, args: List(String)) -> String {
  shell_words([executable, ..args])
}

fn shell_words(words: List(String)) -> String {
  words |> list.map(shell_word) |> string.join(with: " ")
}

fn shell_word(value: String) -> String {
  case value != "" && all(string.to_graphemes(value), is_shell_safe_char) {
    True -> value
    False -> shell_quote(value)
  }
}

fn is_shell_safe_char(ch: String) -> Bool {
  is_between(ch, "a", "z")
  || is_between(ch, "A", "Z")
  || is_between(ch, "0", "9")
  || list.contains(["/", ".", "-", "_", ":", "=", "+", ","], ch)
}

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

type TaskUpdateStates {
  TaskUpdateStates(
    claim: Option(workflow_completion_policy.LinearStateRef),
    success: Option(workflow_completion_policy.LinearStateRef),
    no_review_success: Option(workflow_completion_policy.LinearStateRef),
    failure: Option(workflow_completion_policy.LinearStateRef),
    partial_success: Option(workflow_completion_policy.LinearStateRef),
  )
}

type TaskUpdateComments {
  TaskUpdateComments(claim: Bool, success: Bool, failure: Bool, park: Bool)
}

type TaskUpdateResultConfig {
  TaskUpdateResultConfig(
    include_result_on_success: Bool,
    attach_result_on_success: Bool,
    force_success_comment: Bool,
    result_max_chars: Int,
  )
}

type TaskUpdateResultMode {
  ResultNone
  ResultComment
  ResultAttachment
}

fn resolve_handoff(
  root: yay.Node,
) -> Result(config_types.HandoffConfig, error.ConfigError) {
  case get_node(root, "task_updates") {
    None -> Ok(default_handoff_config())
    Some(yay.NodeMap(_) as task_updates) -> resolve_task_updates(task_updates)
    Some(_) -> Error(error.InvalidConfig("task_updates must be a map"))
  }
}

fn resolve_task_updates(
  task_updates: yay.Node,
) -> Result(config_types.HandoffConfig, error.ConfigError) {
  use _ <- result.try(
    reject_unknown_map_keys(task_updates, "task_updates", [
      "enabled",
      "states",
      "workflows",
      "comment_on",
      "result",
    ]),
  )
  use enabled_option <- result.try(get_bool_strict(
    task_updates,
    "enabled",
    "task_updates.enabled",
  ))
  let enabled = enabled_option |> bool_default(False)
  use states <- result.try(resolve_task_update_states(task_updates))
  use workflow_overrides <- result.try(resolve_task_update_workflow_overrides(
    task_updates,
  ))
  use comments <- result.try(resolve_task_update_comments(task_updates))
  use result_config <- result.try(resolve_task_update_result(task_updates))
  use completion_states <- result.try(resolve_task_update_completion_states(
    states,
    workflow_overrides,
  ))
  Ok(config_types.HandoffConfig(
    enabled: enabled,
    comment_on_claim: comments.claim,
    comment_on_success: comments.success || result_config.force_success_comment,
    comment_on_failure: comments.failure,
    comment_on_park: comments.park,
    claim_state_id: states.claim,
    success_state_id: states.success,
    failure_state_id: states.failure,
    include_result_on_success: result_config.include_result_on_success,
    attach_result_on_success: result_config.attach_result_on_success,
    attachment_fallback_to_markdown_link: True,
    result_max_chars: result_config.result_max_chars,
    completion_states: completion_states,
  ))
}

fn resolve_task_update_states(
  task_updates: yay.Node,
) -> Result(TaskUpdateStates, error.ConfigError) {
  use states <- result.try(get_map_strict_or_empty(
    task_updates,
    "states",
    "task_updates.states",
  ))
  use _ <- result.try(
    reject_unknown_map_keys(states, "task_updates.states", [
      "claim",
      "success",
      "no_review_success",
      "failure",
      "partial_success",
    ]),
  )
  use claim <- result.try(read_task_update_state(
    states,
    "claim",
    "task_updates.states.claim",
  ))
  use success <- result.try(read_task_update_state(
    states,
    "success",
    "task_updates.states.success",
  ))
  use no_review_success <- result.try(read_task_update_state(
    states,
    "no_review_success",
    "task_updates.states.no_review_success",
  ))
  use failure <- result.try(read_task_update_state(
    states,
    "failure",
    "task_updates.states.failure",
  ))
  use partial_success <- result.try(read_task_update_state(
    states,
    "partial_success",
    "task_updates.states.partial_success",
  ))
  Ok(TaskUpdateStates(
    claim: claim,
    success: success,
    no_review_success: no_review_success,
    failure: failure,
    partial_success: partial_success,
  ))
}

fn resolve_task_update_workflow_overrides(
  task_updates: yay.Node,
) -> Result(
  dict.Dict(String, workflow_completion_policy.WorkflowCompletionOverride),
  error.ConfigError,
) {
  use workflows <- result.try(get_map_strict_or_empty(
    task_updates,
    "workflows",
    "task_updates.workflows",
  ))
  case workflows {
    yay.NodeMap(entries) ->
      read_task_update_workflow_override_entries(entries, [])
    _ -> Ok(dict.new())
  }
}

fn read_task_update_workflow_override_entries(
  entries: List(#(yay.Node, yay.Node)),
  acc: List(#(String, workflow_completion_policy.WorkflowCompletionOverride)),
) -> Result(
  dict.Dict(String, workflow_completion_policy.WorkflowCompletionOverride),
  error.ConfigError,
) {
  case entries {
    [] -> Ok(dict.from_list(list.reverse(acc)))
    [#(yay.NodeStr(key), yay.NodeMap(_) as workflow), ..rest] -> {
      let workflow_id = normalize_label(key)
      case valid_workflow_name(workflow_id) {
        False ->
          Error(error.InvalidConfig(
            "task_updates.workflows has invalid workflow id: " <> key,
          ))
        True -> {
          use override <- result.try(resolve_task_update_workflow_override(
            workflow,
            "task_updates.workflows." <> key,
          ))
          read_task_update_workflow_override_entries(rest, [
            #(workflow_id, override),
            ..acc
          ])
        }
      }
    }
    [#(yay.NodeStr(key), _), ..] ->
      Error(error.InvalidConfig(
        "task_updates.workflows." <> key <> " must be a map",
      ))
    [#(_, _), ..] ->
      Error(error.InvalidConfig("task_updates.workflows keys must be strings"))
  }
}

fn resolve_task_update_workflow_override(
  workflow: yay.Node,
  path: String,
) -> Result(
  workflow_completion_policy.WorkflowCompletionOverride,
  error.ConfigError,
) {
  use _ <- result.try(
    reject_unknown_map_keys(workflow, path, [
      "requires_review",
      "states",
    ]),
  )
  use requires_review <- result.try(get_bool_strict(
    workflow,
    "requires_review",
    path <> ".requires_review",
  ))
  use states <- result.try(get_map_strict_or_empty(
    workflow,
    "states",
    path <> ".states",
  ))
  use _ <- result.try(
    reject_unknown_map_keys(states, path <> ".states", [
      "success",
      "no_review_success",
      "failure",
      "partial_success",
    ]),
  )
  use success <- result.try(read_task_update_state(
    states,
    "success",
    path <> ".states.success",
  ))
  use no_review_success <- result.try(read_task_update_state(
    states,
    "no_review_success",
    path <> ".states.no_review_success",
  ))
  use failure <- result.try(read_task_update_state(
    states,
    "failure",
    path <> ".states.failure",
  ))
  use partial_success <- result.try(read_task_update_state(
    states,
    "partial_success",
    path <> ".states.partial_success",
  ))
  Ok(
    workflow_completion_policy.WorkflowCompletionOverride(
      ..workflow_completion_policy.default_override(),
      produces_reviewable_artifacts: requires_review,
      requires_review: requires_review,
      success_state: success,
      no_review_completion_state: no_review_success,
      failure_state: failure,
      partial_success_state: partial_success,
    ),
  )
}

fn read_task_update_state(
  states: yay.Node,
  key: String,
  path: String,
) -> Result(
  Option(workflow_completion_policy.LinearStateRef),
  error.ConfigError,
) {
  use value <- result.try(get_optional_string_strict(states, key, path))
  case value {
    None -> Ok(None)
    Some(value) -> task_update_state_ref(value, path)
  }
}

fn task_update_state_ref(
  value: String,
  path: String,
) -> Result(
  Option(workflow_completion_policy.LinearStateRef),
  error.ConfigError,
) {
  let value = string.trim(value)
  case value == "" {
    True -> Error(error.InvalidConfig(path <> " must be non-empty"))
    False -> Ok(Some(workflow_completion_policy.StateByName(value)))
  }
}

fn resolve_task_update_completion_states(
  states: TaskUpdateStates,
  workflow_overrides: dict.Dict(
    String,
    workflow_completion_policy.WorkflowCompletionOverride,
  ),
) -> Result(
  Option(workflow_completion_policy.CompletionStatePolicy),
  error.ConfigError,
) {
  case
    states.no_review_success,
    states.partial_success,
    dict.size(workflow_overrides)
  {
    None, None, 0 -> Ok(None)
    _, _, _ -> {
      use success <- result.try(required_global_completion_state(
        states.success,
        "task_updates.states.success",
        states,
      ))
      use failure <- result.try(required_global_completion_state(
        states.failure,
        "task_updates.states.failure",
        states,
      ))
      let partial_success = case states.partial_success {
        Some(state) -> Some(state)
        None -> failure
      }
      Ok(
        Some(workflow_completion_policy.CompletionStatePolicy(
          default_completion_state: success,
          no_review_completion_state: states.no_review_success,
          failure_state: failure,
          partial_success_state: partial_success,
          cancellation_state: None,
          workflows: workflow_overrides,
        )),
      )
    }
  }
}

fn required_global_completion_state(
  state: Option(workflow_completion_policy.LinearStateRef),
  path: String,
  states: TaskUpdateStates,
) -> Result(
  Option(workflow_completion_policy.LinearStateRef),
  error.ConfigError,
) {
  case state {
    Some(_) -> Ok(state)
    None ->
      case global_completion_states_require_defaults(states) {
        True ->
          Error(error.InvalidConfig(
            path
            <> " is required when task_updates.states.no_review_success or task_updates.states.partial_success is set",
          ))
        False -> Ok(None)
      }
  }
}

fn global_completion_states_require_defaults(states: TaskUpdateStates) -> Bool {
  case states.no_review_success, states.partial_success {
    None, None -> False
    _, _ -> True
  }
}

fn resolve_task_update_comments(
  task_updates: yay.Node,
) -> Result(TaskUpdateComments, error.ConfigError) {
  case get_node(task_updates, "comment_on") {
    None ->
      Ok(TaskUpdateComments(
        claim: False,
        success: False,
        failure: False,
        park: False,
      ))
    Some(yay.NodeSeq(values)) -> {
      use events <- result.try(read_task_update_comment_events(values, []))
      Ok(TaskUpdateComments(
        claim: list.contains(events, "claim"),
        success: list.contains(events, "success"),
        failure: list.contains(events, "failure"),
        park: list.contains(events, "park"),
      ))
    }
    Some(_) ->
      Error(error.InvalidConfig("task_updates.comment_on must be a string list"))
  }
}

fn read_task_update_comment_events(
  values: List(yay.Node),
  acc: List(String),
) -> Result(List(String), error.ConfigError) {
  case values {
    [] -> Ok(acc)
    [yay.NodeStr(value), ..rest] -> {
      let event = value |> string.trim |> string.lowercase
      case list.contains(["claim", "success", "failure", "park"], event) {
        True -> read_task_update_comment_events(rest, [event, ..acc])
        False ->
          Error(error.InvalidConfig(
            "task_updates.comment_on has invalid event: "
            <> event
            <> "; supported events are claim, success, failure, and park",
          ))
      }
    }
    [_, ..] ->
      Error(error.InvalidConfig(
        "task_updates.comment_on entries must be strings",
      ))
  }
}

fn resolve_task_update_result(
  task_updates: yay.Node,
) -> Result(TaskUpdateResultConfig, error.ConfigError) {
  use result_node <- result.try(get_map_strict_or_empty(
    task_updates,
    "result",
    "task_updates.result",
  ))
  use _ <- result.try(
    reject_unknown_map_keys(result_node, "task_updates.result", [
      "on_success",
      "max_chars",
    ]),
  )
  use mode <- result.try(resolve_task_update_result_mode(result_node))
  use max_chars <- result.try(resolve_task_update_result_max_chars(result_node))
  case mode {
    ResultNone ->
      Ok(TaskUpdateResultConfig(
        include_result_on_success: False,
        attach_result_on_success: False,
        force_success_comment: False,
        result_max_chars: max_chars,
      ))
    ResultComment ->
      Ok(TaskUpdateResultConfig(
        include_result_on_success: True,
        attach_result_on_success: False,
        force_success_comment: True,
        result_max_chars: max_chars,
      ))
    ResultAttachment ->
      Ok(TaskUpdateResultConfig(
        include_result_on_success: False,
        attach_result_on_success: True,
        force_success_comment: False,
        result_max_chars: max_chars,
      ))
  }
}

fn resolve_task_update_result_mode(
  result_node: yay.Node,
) -> Result(TaskUpdateResultMode, error.ConfigError) {
  use raw <- result.try(get_string_strict(
    result_node,
    "on_success",
    "task_updates.result.on_success",
  ))
  case raw {
    None -> Ok(ResultNone)
    Some(value) ->
      case value |> string.trim |> string.lowercase {
        "none" -> Ok(ResultNone)
        "comment" -> Ok(ResultComment)
        "attachment" -> Ok(ResultAttachment)
        other ->
          Error(error.InvalidConfig(
            "task_updates.result.on_success must be one of none, comment, or attachment; got "
            <> other,
          ))
      }
  }
}

fn resolve_task_update_result_max_chars(
  result_node: yay.Node,
) -> Result(Int, error.ConfigError) {
  use configured <- result.try(get_int_strict(
    result_node,
    "max_chars",
    "task_updates.result.max_chars",
  ))
  let max_chars = configured |> int_default(8000)
  case max_chars <= 0 {
    True ->
      Error(error.InvalidConfig(
        "task_updates.result.max_chars must be positive",
      ))
    False -> Ok(max_chars)
  }
}

fn resolve_linear_contract(
  root: yay.Node,
) -> Result(config_types.LinearContractConfig, error.ConfigError) {
  let defaults = default_linear_contract_config()
  use simplified_fields <- result.try(
    tracker_config.resolve_root_linear_contract_fields(root),
  )
  case get_node(root, "linear_contract") {
    None ->
      Ok(tracker_config.apply_root_linear_contract_fields(
        defaults,
        simplified_fields,
      ))
    Some(_) ->
      Error(error.InvalidConfig(
        "linear_contract was removed. Use tracker.linear.check_setup. See docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md.",
      ))
  }
}

fn resolve_linear_commands(
  root: yay.Node,
) -> Result(config_types.LinearCommandConfig, error.ConfigError) {
  case get_node(root, "remote_commands") {
    Some(_) ->
      Error(error.InvalidConfig(
        "remote_commands was removed. Remove this section; use scherzoctl for operator control. See docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md.",
      ))
    None ->
      case get_node(root, "linear_commands") {
        Some(_) ->
          Error(error.InvalidConfig(
            "linear_commands was removed. Remove this section; use scherzoctl for operator control. See docs/specs/SCHERZO_YAML_SIMPLIFIED_V1.md.",
          ))
        None -> Ok(default_linear_command_config())
      }
  }
}

fn resolve_ui_server(
  root: yay.Node,
  _env: Env,
) -> Result(config_types.UiServerConfig, error.ConfigError) {
  let defaults = default_ui_server_config()
  case get_node(root, "ui_server") {
    None -> Ok(defaults)
    Some(node) ->
      case node {
        yay.NodeMap(_) -> {
          use _ <- result.try(reject_removed_ui_server_keys(node))
          use _ <- result.try(
            reject_unknown_map_keys(node, "ui_server", [
              "enabled",
              "endpoint",
              "credential_ref",
              "daemon_label",
              "command_bridge_enabled",
              "heartbeat_interval_ms",
              "state_interval_ms",
              "retry_initial_ms",
              "retry_max_ms",
            ]),
          )
          use enabled_option <- result.try(get_bool_strict(
            node,
            "enabled",
            "ui_server.enabled",
          ))
          use endpoint_option <- result.try(get_optional_string_strict(
            node,
            "endpoint",
            "ui_server.endpoint",
          ))
          use credential_ref_option <- result.try(get_optional_string_strict(
            node,
            "credential_ref",
            "ui_server.credential_ref",
          ))
          use daemon_label_option <- result.try(get_optional_string_strict(
            node,
            "daemon_label",
            "ui_server.daemon_label",
          ))
          use command_bridge_enabled_option <- result.try(get_bool_strict(
            node,
            "command_bridge_enabled",
            "ui_server.command_bridge_enabled",
          ))
          use heartbeat_interval_ms_option <- result.try(get_int_strict(
            node,
            "heartbeat_interval_ms",
            "ui_server.heartbeat_interval_ms",
          ))
          use state_interval_ms_option <- result.try(get_int_strict(
            node,
            "state_interval_ms",
            "ui_server.state_interval_ms",
          ))
          use retry_initial_ms_option <- result.try(get_int_strict(
            node,
            "retry_initial_ms",
            "ui_server.retry_initial_ms",
          ))
          use retry_max_ms_option <- result.try(get_int_strict(
            node,
            "retry_max_ms",
            "ui_server.retry_max_ms",
          ))
          let enabled = enabled_option |> bool_default(False)
          let endpoint = endpoint_option |> optional_non_empty_string
          let credential_ref =
            credential_ref_option |> optional_non_empty_string
          let command_bridge_enabled =
            command_bridge_enabled_option |> bool_default(False)
          let heartbeat_interval_ms =
            heartbeat_interval_ms_option |> int_default(5000)
          let state_interval_ms = state_interval_ms_option |> int_default(5000)
          let retry_initial_ms = retry_initial_ms_option |> int_default(500)
          let retry_max_ms = retry_max_ms_option |> int_default(30_000)
          use _ <- result.try(validate_ui_server_timing(
            heartbeat_interval_ms,
            state_interval_ms,
            retry_initial_ms,
            retry_max_ms,
          ))
          let endpoint = endpoint |> option.map(normalize_ui_server_endpoint)
          use credential_ref <- result.try(
            credential_ref
            |> option.map(normalize_ui_server_credential_ref)
            |> collapse_result_option,
          )
          use daemon_label <- result.try(
            daemon_label_option
            |> option.map(normalize_ui_server_daemon_label)
            |> collapse_result_option,
          )
          case enabled {
            False ->
              Ok(config_types.UiServerDisabled(
                endpoint: endpoint,
                credential_ref: credential_ref,
                daemon_label: daemon_label,
              ))
            True -> {
              use endpoint <- result.try(required_option(
                endpoint,
                error.InvalidConfig(
                  "ui_server.endpoint is required when enabled",
                ),
              ))
              use _ <- result.try(validate_ui_server_endpoint(endpoint))
              use credential_ref <- result.try(required_option(
                credential_ref,
                error.InvalidConfig(
                  "ui_server.credential_ref is required when enabled",
                ),
              ))
              Ok(config_types.UiServerEnabled(
                endpoint: endpoint,
                credential_ref: credential_ref,
                daemon_label: daemon_label,
                command_bridge_enabled: command_bridge_enabled,
                heartbeat_interval_ms: heartbeat_interval_ms,
                state_interval_ms: state_interval_ms,
                retry_initial_ms: retry_initial_ms,
                retry_max_ms: retry_max_ms,
              ))
            }
          }
        }
        _ -> Error(error.InvalidConfig("ui_server must be a map"))
      }
  }
}

fn reject_removed_ui_server_keys(
  node: yay.Node,
) -> Result(Nil, error.ConfigError) {
  case get_node(node, "enrollment_token_env") {
    Some(_) ->
      Error(error.InvalidConfig(
        "ui_server.enrollment_token_env was removed. Pair with scherzo connect and use ui_server.credential_ref instead.",
      ))
    None ->
      case get_node(node, "enrollment_token") {
        Some(_) ->
          Error(error.InvalidConfig(
            "ui_server.enrollment_token was removed. Pair with scherzo connect and use ui_server.credential_ref instead.",
          ))
        None ->
          case get_node(node, "credential") {
            Some(_) ->
              Error(error.InvalidConfig(
                "ui_server.credential is not supported; store daemon credentials outside project YAML and use ui_server.credential_ref instead.",
              ))
            None ->
              case get_node(node, "daemon_credential") {
                Some(_) ->
                  Error(error.InvalidConfig(
                    "ui_server.daemon_credential is not supported; store daemon credentials outside project YAML and use ui_server.credential_ref instead.",
                  ))
                None -> Ok(Nil)
              }
          }
      }
  }
}

fn validate_ui_server_endpoint(
  endpoint: String,
) -> Result(Nil, error.ConfigError) {
  case uri.parse(endpoint) {
    Error(_) -> Error(invalid_ui_server_endpoint_error())
    Ok(parsed) -> {
      let uri.Uri(
        scheme: scheme,
        userinfo: userinfo,
        host: host,
        query: query,
        fragment: fragment,
        ..,
      ) = parsed
      case scheme, userinfo, host, query, fragment {
        Some("https"), None, Some(host), None, None ->
          case valid_ui_server_host(host) {
            True -> Ok(Nil)
            False -> Error(invalid_ui_server_endpoint_error())
          }
        Some("http"), None, Some(host), None, None ->
          case valid_ui_server_host(host) && loopback_ui_server_host(host) {
            True -> Ok(Nil)
            False -> Error(invalid_ui_server_endpoint_error())
          }
        _, _, _, _, _ -> Error(invalid_ui_server_endpoint_error())
      }
    }
  }
}

fn valid_ui_server_host(host: String) -> Bool {
  host != "" && host != "0.0.0.0" && host != "::"
}

fn loopback_ui_server_host(host: String) -> Bool {
  host == "localhost" || host == "127.0.0.1" || host == "::1"
}

fn invalid_ui_server_endpoint_error() -> error.ConfigError {
  error.InvalidConfig(
    "ui_server.endpoint must use https, or http only for loopback development URLs; it must include a host and no query, fragment, or userinfo",
  )
}

fn validate_ui_server_timing(
  heartbeat_interval_ms: Int,
  state_interval_ms: Int,
  retry_initial_ms: Int,
  retry_max_ms: Int,
) -> Result(Nil, error.ConfigError) {
  case
    heartbeat_interval_ms <= 0
    || state_interval_ms <= 0
    || retry_initial_ms <= 0
    || retry_max_ms <= 0
  {
    True ->
      Error(error.InvalidConfig(
        "ui_server heartbeat and retry timing values must be positive integers",
      ))
    False ->
      case retry_max_ms < retry_initial_ms {
        True ->
          Error(error.InvalidConfig(
            "ui_server.retry_max_ms must be greater than or equal to ui_server.retry_initial_ms",
          ))
        False -> Ok(Nil)
      }
  }
}

fn normalize_ui_server_endpoint(value: String) -> String {
  trim_trailing_slashes(string.trim(value))
}

fn trim_trailing_slashes(value: String) -> String {
  case
    value != "https://" && value != "http://" && string.ends_with(value, "/")
  {
    True -> trim_trailing_slashes(string.drop_end(value, 1))
    False -> value
  }
}

fn normalize_ui_server_credential_ref(
  value: String,
) -> Result(String, error.ConfigError) {
  case remote_config_validation.normalize_credential_ref(value) {
    Ok(profile) -> Ok(profile)
    Error(validation_error) ->
      Error(
        error.InvalidConfig(
          remote_config_validation.credential_ref_error_message(
            validation_error,
          ),
        ),
      )
  }
}

fn normalize_ui_server_daemon_label(
  value: String,
) -> Result(String, error.ConfigError) {
  case remote_config_validation.normalize_daemon_label(value) {
    Ok(label) -> Ok(label)
    Error(validation_error) ->
      Error(error.InvalidConfig(
        "ui_server.daemon_label "
        <> remote_config_validation.daemon_label_error_message(validation_error),
      ))
  }
}

fn collapse_result_option(
  value: Option(Result(a, error.ConfigError)),
) -> Result(Option(a), error.ConfigError) {
  case value {
    None -> Ok(None)
    Some(Ok(value)) -> Ok(Some(value))
    Some(Error(error)) -> Error(error)
  }
}

fn resolve_workspace_profiles(
  root: yay.Node,
) -> Result(config_types.WorkspaceHookProfiles, error.ConfigError) {
  root
  |> get_map("workspace")
  |> tracker_config.resolve_workspace_drivers
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
        "workspace.driver references unknown driver: "
        <> workspace_profiles.default_profile,
      ))
  }
}

fn resolve_artifact_limits(
  root: yay.Node,
) -> Result(config_types.ArtifactLimits, error.ConfigError) {
  use artifacts <- result.try(get_map_strict_or_empty(
    root,
    "artifacts",
    "artifacts",
  ))
  use limits <- result.try(get_map_strict_or_empty(
    artifacts,
    "limits",
    "artifacts.limits",
  ))
  use _ <- result.try(reject_removed_artifact_limit_keys(limits))
  let command_stream_max_chars =
    get_int(limits, "command_output_chars") |> int_default(20_000)
  let template_field_max_chars =
    get_int(limits, "template_field_chars") |> int_default(8000)
  let workflow_summary_max_chars =
    get_int(limits, "workflow_summary_chars") |> int_default(20_000)
  case
    command_stream_max_chars <= 0
    || template_field_max_chars <= 0
    || workflow_summary_max_chars <= 0
  {
    True ->
      Error(error.InvalidConfig("artifacts.limits values must be positive"))
    False ->
      Ok(config_types.ArtifactLimits(
        command_stream_max_chars: command_stream_max_chars,
        template_field_max_chars: template_field_max_chars,
        workflow_summary_max_chars: workflow_summary_max_chars,
      ))
  }
}

fn reject_removed_artifact_limit_keys(
  limits: yay.Node,
) -> Result(Nil, error.ConfigError) {
  case get_node(limits, "command_stream_max_chars") {
    Some(_) ->
      Error(config_migration_hint(
        "artifacts.limits.command_stream_max_chars",
        "artifacts.limits.command_output_chars",
      ))
    None ->
      case get_node(limits, "template_field_max_chars") {
        Some(_) ->
          Error(config_migration_hint(
            "artifacts.limits.template_field_max_chars",
            "artifacts.limits.template_field_chars",
          ))
        None ->
          case get_node(limits, "workflow_summary_max_chars") {
            Some(_) ->
              Error(config_migration_hint(
                "artifacts.limits.workflow_summary_max_chars",
                "artifacts.limits.workflow_summary_chars",
              ))
            None -> Ok(Nil)
          }
      }
  }
}

fn resolve_artifact_repositories(
  root: yay.Node,
) -> Result(artifact_publication_config.ArtifactRepositories, error.ConfigError) {
  artifact_publication_config.parse_root_repositories(root)
  |> result.map_error(fn(parse_error) {
    error.InvalidConfig(artifact_publication_config.error_message(parse_error))
  })
}

fn resolve_workflow_model_settings(
  root: yay.Node,
) -> Result(model_config.Settings, error.ConfigError) {
  let agents = get_map(root, "agents")
  model_config.read_settings(
    agents,
    model_config.SettingsPaths(
      provider_path: "agents.provider",
      provider_model_path: "agents.model",
      model_path: "agents.model",
      thinking_path: "agents.thinking",
    ),
    fn(_code, message) { error.InvalidConfig(message) },
  )
}

fn resolve_scheduled_jobs(
  root: yay.Node,
  routing: config_types.RoutingConfig,
) -> Result(List(config_types.ScheduledJobConfig), error.ConfigError) {
  case get_node(root, "schedules") {
    None -> Ok([])
    Some(yay.NodeSeq(values)) ->
      read_scheduled_job_values(values, routing, [], [])
    Some(_) -> Error(error.InvalidConfig("schedules must be a list"))
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
            "schedules has duplicate schedule id: " <> job.id,
          ))
        False ->
          read_scheduled_job_values(rest, routing, [job.id, ..seen_ids], [
            job,
            ..acc
          ])
      }
    }
    [_, ..] -> Error(error.InvalidConfig("schedules entries must be maps"))
  }
}

fn resolve_scheduled_job(
  node: yay.Node,
  routing: config_types.RoutingConfig,
) -> Result(config_types.ScheduledJobConfig, error.ConfigError) {
  use _ <- result.try(reject_schedule_payload_fields(node))
  use workflow_raw <- result.try(get_required_string(
    node,
    "workflow",
    error.InvalidConfig("schedules entries require workflow"),
  ))
  let workflow = normalize_label(workflow_raw)
  use _ <- result.try(validate_scheduled_id(workflow, "schedules.workflow"))
  use id_option <- result.try(get_optional_string_strict(
    node,
    "id",
    "schedules." <> workflow <> ".id",
  ))
  let id =
    id_option
    |> option.map(normalize_label)
    |> option.unwrap(workflow)
  use _ <- result.try(validate_scheduled_id(id, "schedules.id"))
  use enabled_option <- result.try(get_bool_strict(
    node,
    "enabled",
    "schedules." <> id <> ".enabled",
  ))
  let enabled = enabled_option |> bool_default(True)
  use every_ms <- result.try(resolve_scheduled_every(node, id, enabled))
  use overlap <- result.try(resolve_scheduled_overlap(node, id))
  use catch_up <- result.try(resolve_scheduled_catch_up(node, id))
  use on_failure <- result.try(resolve_scheduled_failure(node, id))
  case enabled && !dict.has_key(routing.workflows, workflow) {
    True ->
      Error(error.InvalidConfig(
        "schedules."
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
  case get_string_strict(node, "every", "schedules." <> id <> ".every") {
    Error(err) -> Error(err)
    Ok(None) ->
      case enabled {
        True ->
          Error(error.InvalidConfig(
            "schedules." <> id <> ".every is required when enabled",
          ))
        False -> Ok(0)
      }
    Ok(Some(value)) -> {
      use every_ms <- result.try(duration_config.scheduled_every_ms(
        value,
        "schedules." <> id <> ".every",
      ))
      case enabled && every_ms < 1000 {
        True ->
          Error(error.InvalidConfig(
            "schedules." <> id <> ".every must be at least 1000ms when enabled",
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
  case get_string_strict(node, "overlap", "schedules." <> id <> ".overlap") {
    Error(err) -> Error(err)
    Ok(None) -> Ok(config_types.SkipOverlap)
    Ok(Some(value)) ->
      case value |> string.trim |> string.lowercase {
        "skip" -> Ok(config_types.SkipOverlap)
        other ->
          Error(error.InvalidScheduledJobOverlap(
            "schedules."
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
    "schedules." <> id <> ".catch_up",
  ))
  case catch_up |> bool_default(False) {
    False -> Ok(False)
    True ->
      Error(error.ScheduledJobCatchUpUnsupported(
        "schedules." <> id <> ".catch_up=true is not supported in the MVP",
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
        "schedules." <> id <> ".on_failure must be a map",
      ))
  }
}

fn resolve_scheduled_on_failure_map(
  node: yay.Node,
  id: String,
) -> Result(config_types.ScheduledFailureConfig, error.ConfigError) {
  case get_node(node, "linear") {
    Some(_) ->
      Error(config_migration_hint(
        "schedules." <> id <> ".on_failure.linear",
        "schedules." <> id <> ".on_failure.task",
      ))
    None ->
      case get_node(node, "task") {
        None -> Ok(default_scheduled_failure_config())
        Some(yay.NodeMap(_)) -> {
          use task <- result.try(resolve_scheduled_task_failure(
            get_map(node, "task"),
            id,
          ))
          Ok(config_types.ScheduledFailureConfig(task: task))
        }
        Some(_) ->
          Error(error.InvalidConfig(
            "schedules." <> id <> ".on_failure.task must be a map",
          ))
      }
  }
}

fn resolve_scheduled_task_failure(
  node: yay.Node,
  id: String,
) -> Result(config_types.ScheduledTaskFailureConfig, error.ConfigError) {
  let path = "schedules." <> id <> ".on_failure.task"
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
        path <> ".state is required when failure task reporting is enabled",
      ))
    _, _ ->
      Ok(config_types.ScheduledTaskFailureConfig(
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
    Ok(None) -> Ok(config_types.OpenTaskPerSchedule)
    Ok(Some(value)) ->
      case value |> string.trim |> string.lowercase {
        "open_task_per_schedule" -> Ok(config_types.OpenTaskPerSchedule)
        "open_issue_per_job" ->
          Error(config_migration_hint(
            path <> ".dedupe: open_issue_per_job",
            path <> ".dedupe: open_task_per_schedule",
          ))
        other ->
          Error(error.InvalidConfig(
            path
            <> ".dedupe unsupported value "
            <> other
            <> "; the MVP accepts only open_task_per_schedule",
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
            "schedules."
            <> key
            <> " is intentionally deferred; put schedule-specific details in workflow YAML, prompt files, scripts, environment, or repository config",
          ))
        False -> reject_schedule_payload_entries(rest)
      }
    [_, ..rest] -> reject_schedule_payload_entries(rest)
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

fn ui_policy_at(
  value: Option(String),
  path: String,
) -> Result(config_types.UiRequestPolicy, error.ConfigError) {
  case value {
    Some(value) ->
      case string.lowercase(string.trim(value)) {
        "cancel" -> Ok(config_types.Cancel)
        "fail" -> Ok(config_types.Fail)
        "ignore" -> Ok(config_types.Ignore)
        "operator" -> Ok(config_types.Operator)
        other -> Error(error.InvalidConfig("invalid " <> path <> ": " <> other))
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

fn reject_unknown_map_keys(
  node: yay.Node,
  path: String,
  allowed_keys: List(String),
) -> Result(Nil, error.ConfigError) {
  case node {
    yay.NodeMap([]) -> Ok(Nil)
    yay.NodeMap([#(yay.NodeStr(key), _), ..rest]) ->
      case list.contains(allowed_keys, key) {
        True -> reject_unknown_map_keys(yay.NodeMap(rest), path, allowed_keys)
        False ->
          Error(error.InvalidConfig(
            path
            <> "."
            <> key
            <> " is not supported; supported keys are "
            <> string.join(allowed_keys, with: ", "),
          ))
      }
    yay.NodeMap([#(_, _), ..]) ->
      Error(error.InvalidConfig(path <> " keys must be strings"))
    _ -> Ok(Nil)
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

fn get_int_strict(
  node: yay.Node,
  key: String,
  path: String,
) -> Result(Option(Int), error.ConfigError) {
  case get_node(node, key) {
    None -> Ok(None)
    Some(yay.NodeInt(value)) -> Ok(Some(value))
    Some(_) -> Error(error.InvalidConfig(path <> " must be an integer"))
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
