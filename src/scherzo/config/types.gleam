import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/model_config
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_completion_policy

pub type ConfigWarning {
  ConfigWarning(event: String, path: String, replacement: String)
}

pub fn config_warning_message(warning: ConfigWarning) -> String {
  let ConfigWarning(event: event, path: path, replacement: replacement) =
    warning
  event <> " path=" <> path <> " replacement=" <> replacement
}

pub type TrackerConfig {
  TrackerConfig(
    kind: tracker_kind.TrackerKind,
    endpoint: String,
    api_key: Option(String),
    project_slug: Option(String),
    active_states: List(issue_state.IssueState),
    dispatch_states: List(issue_state.IssueState),
    terminal_states: List(issue_state.IssueState),
  )
}

pub type PollingConfig {
  PollingConfig(interval_ms: Int)
}

pub type WorkspaceConfig {
  WorkspaceConfig(root: String)
}

pub type HooksConfig {
  HooksConfig(
    after_create: Option(String),
    before_run: Option(String),
    after_run: Option(String),
    before_remove: Option(String),
    timeout_ms: Int,
  )
}

pub type UiRequestPolicy {
  Cancel
  Fail
  Ignore
  Operator
}

pub type AgentConfig {
  AgentConfig(
    max_concurrent_agents: Int,
    max_turns: Int,
    max_retry_backoff_ms: Int,
    max_retry_attempts: Int,
    max_sessions_per_issue: Int,
    context_recovery_max_attempts: Int,
    context_recovery_prompt_char_limit: Int,
    max_concurrent_agents_by_state: Dict(issue_state.IssueStateKey, Int),
  )
}

pub type PiArgvCommand {
  PiArgvCommand(
    executable: String,
    args: List(String),
    env: List(#(String, String)),
  )
}

pub type PiSessionPersistenceConfig {
  PiSessionPersistenceConfig(enabled: Bool, recovery_prompt: String)
}

pub type PiConfig {
  PiConfig(
    command: String,
    turn_timeout_ms: Int,
    read_timeout_ms: Int,
    stall_timeout_ms: Int,
    auto_retry: Bool,
    ui_request_policy: UiRequestPolicy,
    ui_request_timeout_ms: Int,
    compatibility_probe: Bool,
    rate_limit_payload: Option(String),
    argv_command: Option(PiArgvCommand),
    session_persistence: PiSessionPersistenceConfig,
  )
}

pub type UiServerConfig {
  UiServerConfig(
    enabled: Bool,
    endpoint: Option(String),
    enrollment_token_env: Option(String),
    enrollment_token: Option(String),
  )
}

pub type HandoffConfig {
  HandoffConfig(
    enabled: Bool,
    comment_on_claim: Bool,
    comment_on_success: Bool,
    comment_on_failure: Bool,
    comment_on_park: Bool,
    claim_state_id: Option(workflow_completion_policy.LinearStateRef),
    success_state_id: Option(workflow_completion_policy.LinearStateRef),
    failure_state_id: Option(workflow_completion_policy.LinearStateRef),
    include_result_on_success: Bool,
    attach_result_on_success: Bool,
    attachment_fallback_to_markdown_link: Bool,
    result_max_chars: Int,
    completion_states: Option(workflow_completion_policy.CompletionStatePolicy),
  )
}

pub type InvalidWorkflowStateTarget {
  InvalidWorkflowStateId(String)
  InvalidWorkflowStateName(String)
}

pub type LinearContractConfig {
  LinearContractConfig(
    enabled: Bool,
    workflow_label_prefix: String,
    workflow_labels: List(String),
    support_labels: List(String),
    required_states: Dict(String, String),
    handoff_state_bindings: Dict(String, String),
    enforce_issue_workflow_labels: Bool,
    invalid_workflow_state_id: Option(String),
    invalid_workflow_state_target: Option(InvalidWorkflowStateTarget),
    comment_on_invalid_workflow: Bool,
  )
}

pub fn invalid_workflow_state_target_value(
  target: InvalidWorkflowStateTarget,
) -> String {
  case target {
    InvalidWorkflowStateId(value) -> value
    InvalidWorkflowStateName(value) -> value
  }
}

pub fn normalized_invalid_workflow_state_target(
  contract: LinearContractConfig,
) -> Option(InvalidWorkflowStateTarget) {
  case contract.invalid_workflow_state_target {
    Some(target) -> normalize_invalid_workflow_state_target(target)
    None ->
      case contract.invalid_workflow_state_id {
        Some(value) ->
          normalize_invalid_workflow_state_target(InvalidWorkflowStateId(value))
        None -> None
      }
  }
}

fn normalize_invalid_workflow_state_target(
  target: InvalidWorkflowStateTarget,
) -> Option(InvalidWorkflowStateTarget) {
  let value = invalid_workflow_state_target_value(target) |> string.trim
  case value == "" {
    True -> None
    False ->
      case target {
        InvalidWorkflowStateId(_) -> Some(InvalidWorkflowStateId(value))
        InvalidWorkflowStateName(_) -> Some(InvalidWorkflowStateName(value))
      }
  }
}

pub type LinearContractRoutingError {
  LinearContractRoutingPrefixMismatch
  LinearContractRoutingWorkflowLabelsMismatch
}

pub fn linear_contract_routing_error_message(
  error: LinearContractRoutingError,
) -> String {
  case error {
    LinearContractRoutingPrefixMismatch ->
      "linear_contract.workflow_label_prefix must match task_routing.labels.prefix"
    LinearContractRoutingWorkflowLabelsMismatch ->
      "linear_contract.workflow_labels must match issue-dispatched workflows when task_routing.labels.require_exactly_one is true"
  }
}

pub fn resolve_linear_contract_for_routing(
  contract: LinearContractConfig,
  routing: RoutingConfig,
  scheduled_jobs: List(ScheduledJobConfig),
  has_labels: Bool,
  has_prefix: Bool,
) -> Result(LinearContractConfig, LinearContractRoutingError) {
  let workflow_names =
    dict.keys(routing.workflows)
    |> normalize_label_list
    |> list.sort(by: string.compare)
  let scheduled_names =
    scheduled_jobs
    |> list.map(fn(job) { job.workflow })
    |> normalize_label_list
  let issue_workflow_names =
    workflow_names
    |> list.filter(fn(name) { !list.contains(scheduled_names, name) })
  let contract_prefix = case has_prefix {
    True -> contract.workflow_label_prefix
    False -> routing.workflow_label_prefix
  }
  case
    has_prefix
    && contract.workflow_label_prefix != routing.workflow_label_prefix
  {
    True -> Error(LinearContractRoutingPrefixMismatch)
    False ->
      resolve_linear_contract_names(
        contract,
        contract_prefix,
        routing.require_exactly_one_workflow_label,
        has_labels,
        workflow_names,
        issue_workflow_names,
      )
  }
}

fn resolve_linear_contract_names(
  contract: LinearContractConfig,
  contract_prefix: String,
  require_exactly_one_workflow_label: Bool,
  has_labels: Bool,
  workflow_names: List(String),
  issue_workflow_names: List(String),
) -> Result(LinearContractConfig, LinearContractRoutingError) {
  let contract_names =
    contract.workflow_labels
    |> normalize_label_list
    |> list.sort(by: string.compare)
  let should_derive_names =
    require_exactly_one_workflow_label
    || contract.enabled
    || contract.enforce_issue_workflow_labels
  case should_derive_names, has_labels {
    True, False ->
      Ok(
        LinearContractConfig(
          ..contract,
          workflow_label_prefix: contract_prefix,
          workflow_labels: issue_workflow_names,
        ),
      )
    True, True ->
      case
        valid_contract_names(
          contract_names,
          workflow_names,
          issue_workflow_names,
        )
      {
        True ->
          Ok(
            LinearContractConfig(
              ..contract,
              workflow_label_prefix: contract_prefix,
              workflow_labels: contract_names,
            ),
          )
        False -> Error(LinearContractRoutingWorkflowLabelsMismatch)
      }
    _, _ ->
      Ok(
        LinearContractConfig(..contract, workflow_label_prefix: contract_prefix),
      )
  }
}

fn valid_contract_names(
  contract_names: List(String),
  workflow_names: List(String),
  issue_workflow_names: List(String),
) -> Bool {
  list.all(issue_workflow_names, fn(name) {
    list.contains(contract_names, name)
  })
  && list.all(contract_names, fn(name) { list.contains(workflow_names, name) })
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

pub type LinearCommandConfig {
  LinearCommandConfig(
    enabled: Bool,
    prefix: String,
    authorized_user_ids: List(String),
    poll_limit_per_issue: Int,
    max_comments_per_tick: Int,
    acknowledge_success: Bool,
    acknowledge_rejection: Bool,
  )
}

pub type RoutingConfig {
  RoutingConfig(
    workflow_label_prefix: String,
    require_exactly_one_workflow_label: Bool,
    default_workflow: Option(String),
    workflows: Dict(String, String),
  )
}

pub type DagHooksConfig {
  DagHooksConfig(
    create: Option(String),
    before_step: Option(String),
    after_step: Option(String),
    remove: Option(String),
    timeout_ms: Int,
  )
}

pub type WorkspaceLifecycleOperation {
  LifecycleCreate
  LifecycleBeforeStep
  LifecycleAfterStep
  LifecycleRemove
}

pub type WorkspaceCapability {
  WorkspaceStatus
  WorkspaceDiff
  WorkspaceChangedFiles
  WorkspaceAssertOnly
  WorkspaceBaseline
  WorkspaceRefreshBase
  WorkspacePublishChange
}

pub type WorkspaceDriverConfig {
  WorkspaceDriverConfig(
    command: String,
    lifecycle: List(WorkspaceLifecycleOperation),
    capabilities: List(WorkspaceCapability),
    timeout_ms: Int,
    env: List(#(String, String)),
  )
}

pub fn validate_workspace_driver_command(
  command: String,
) -> Result(String, String) {
  let command = string.trim(command)
  case command == "" {
    True -> Error("must be non-empty")
    False -> validate_workspace_driver_command_token(command)
  }
}

fn validate_workspace_driver_command_token(
  command: String,
) -> Result(String, String) {
  case contains_workspace_driver_command_whitespace(command) {
    True -> Error("must be one executable token without whitespace")
    False ->
      case contains_workspace_driver_shell_metacharacter(command) {
        True -> Error("must not contain shell metacharacters")
        False ->
          case uses_supported_workspace_driver_env(command) {
            True -> Ok(command)
            False ->
              Error(
                "may only use $SCHERZO_REPO_ROOT as an environment placeholder",
              )
          }
      }
  }
}

fn contains_workspace_driver_command_whitespace(command: String) -> Bool {
  string.contains(command, " ")
  || string.contains(command, "\n")
  || string.contains(command, "\r")
  || string.contains(command, "\t")
}

fn contains_workspace_driver_shell_metacharacter(command: String) -> Bool {
  string.contains(command, ";")
  || string.contains(command, "&")
  || string.contains(command, "|")
  || string.contains(command, "<")
  || string.contains(command, ">")
  || string.contains(command, "`")
  || string.contains(command, "'")
  || string.contains(command, "\"")
}

fn uses_supported_workspace_driver_env(command: String) -> Bool {
  case string.contains(command, "$") {
    False -> True
    True ->
      command == "$SCHERZO_REPO_ROOT"
      || string.starts_with(command, "$SCHERZO_REPO_ROOT/")
  }
}

pub type WorkspaceProfileSource {
  ConfiguredWorkspaceDriver
  SyntheticDefaultWorkspace
}

pub type WorkspaceHookProfile {
  WorkspaceHookProfile(
    name: String,
    driver: Option(WorkspaceDriverConfig),
    source: WorkspaceProfileSource,
  )
}

pub fn empty_dag_hooks() -> DagHooksConfig {
  DagHooksConfig(
    create: None,
    before_step: None,
    after_step: None,
    remove: None,
    timeout_ms: 60_000,
  )
}

pub fn workspace_lifecycle_operation_from_string(
  value: String,
) -> Result(WorkspaceLifecycleOperation, Nil) {
  case value |> string.trim |> string.lowercase {
    "create" -> Ok(LifecycleCreate)
    "before-step" -> Ok(LifecycleBeforeStep)
    "after-step" -> Ok(LifecycleAfterStep)
    "remove" -> Ok(LifecycleRemove)
    _ -> Error(Nil)
  }
}

pub fn workspace_lifecycle_operation_to_string(
  operation: WorkspaceLifecycleOperation,
) -> String {
  case operation {
    LifecycleCreate -> "create"
    LifecycleBeforeStep -> "before-step"
    LifecycleAfterStep -> "after-step"
    LifecycleRemove -> "remove"
  }
}

pub fn workspace_capability_from_string(
  value: String,
) -> Result(WorkspaceCapability, Nil) {
  case value |> string.trim |> string.lowercase {
    "status" -> Ok(WorkspaceStatus)
    "diff" -> Ok(WorkspaceDiff)
    "changed-files" -> Ok(WorkspaceChangedFiles)
    "assert-only" -> Ok(WorkspaceAssertOnly)
    "baseline" -> Ok(WorkspaceBaseline)
    "refresh-base" -> Ok(WorkspaceRefreshBase)
    "publish-change" -> Ok(WorkspacePublishChange)
    _ -> Error(Nil)
  }
}

pub fn workspace_capability_to_string(
  capability: WorkspaceCapability,
) -> String {
  case capability {
    WorkspaceStatus -> "status"
    WorkspaceDiff -> "diff"
    WorkspaceChangedFiles -> "changed-files"
    WorkspaceAssertOnly -> "assert-only"
    WorkspaceBaseline -> "baseline"
    WorkspaceRefreshBase -> "refresh-base"
    WorkspacePublishChange -> "publish-change"
  }
}

pub fn canonical_lifecycle_operations(
  operations: List(WorkspaceLifecycleOperation),
) -> List(WorkspaceLifecycleOperation) {
  [LifecycleCreate, LifecycleBeforeStep, LifecycleAfterStep, LifecycleRemove]
  |> list.filter(fn(operation) { list.contains(operations, operation) })
}

pub fn canonical_workspace_capabilities(
  capabilities: List(WorkspaceCapability),
) -> List(WorkspaceCapability) {
  [
    WorkspaceStatus,
    WorkspaceDiff,
    WorkspaceChangedFiles,
    WorkspaceAssertOnly,
    WorkspaceBaseline,
    WorkspaceRefreshBase,
    WorkspacePublishChange,
  ]
  |> list.filter(fn(capability) { list.contains(capabilities, capability) })
}

pub fn workspace_capability_names(
  capabilities: List(WorkspaceCapability),
) -> List(String) {
  capabilities
  |> canonical_workspace_capabilities
  |> list.map(workspace_capability_to_string)
}

pub fn workspace_capabilities_to_string(
  capabilities: List(WorkspaceCapability),
) -> String {
  case workspace_capability_names(capabilities) {
    [] -> "none"
    names -> string.join(names, with: ", ")
  }
}

pub type WorkspaceHookProfiles {
  WorkspaceHookProfiles(
    default_profile: String,
    profiles: Dict(String, WorkspaceHookProfile),
  )
}

pub type ArtifactLimits {
  ArtifactLimits(
    command_stream_max_chars: Int,
    template_field_max_chars: Int,
    workflow_summary_max_chars: Int,
  )
}

pub type ScheduledOverlap {
  SkipOverlap
}

pub type ScheduledFailureDedupe {
  OpenTaskPerSchedule
}

pub type ScheduledTaskFailureConfig {
  ScheduledTaskFailureConfig(
    enabled: Bool,
    state: Option(String),
    labels: List(String),
    dedupe: ScheduledFailureDedupe,
  )
}

pub type ScheduledFailureConfig {
  ScheduledFailureConfig(task: ScheduledTaskFailureConfig)
}

pub type ScheduledJobConfig {
  ScheduledJobConfig(
    id: String,
    workflow: String,
    enabled: Bool,
    every_ms: Int,
    overlap: ScheduledOverlap,
    catch_up: Bool,
    on_failure: ScheduledFailureConfig,
  )
}

pub type OrchestratorConfig {
  OrchestratorConfig(
    effective: EffectiveConfig,
    config_dir: String,
    routing: RoutingConfig,
    dag_hooks: DagHooksConfig,
    workspace_profiles: WorkspaceHookProfiles,
    artifact_limits: ArtifactLimits,
    model_settings: model_config.Settings,
    scheduled_jobs: List(ScheduledJobConfig),
  )
}

pub type EffectiveConfig {
  EffectiveConfig(
    tracker: TrackerConfig,
    polling: PollingConfig,
    workspace: WorkspaceConfig,
    hooks: HooksConfig,
    agent: AgentConfig,
    pi: PiConfig,
    handoff: HandoffConfig,
    linear_contract: LinearContractConfig,
    linear_commands: LinearCommandConfig,
    ui_server: UiServerConfig,
  )
}

pub fn with_additional_active_state(
  config: EffectiveConfig,
  state: issue_state.IssueState,
) -> EffectiveConfig {
  EffectiveConfig(
    ..config,
    tracker: TrackerConfig(..config.tracker, active_states: [
      state,
      ..config.tracker.active_states
    ]),
  )
}

pub type ResolveReport {
  ResolveReport(config: EffectiveConfig, warnings: List(ConfigWarning))
}

pub fn with_pi_env(
  config: EffectiveConfig,
  env: List(#(String, String)),
) -> EffectiveConfig {
  case env {
    [] -> config
    _ ->
      EffectiveConfig(
        ..config,
        pi: PiConfig(
          ..config.pi,
          command: shell_command_with_env(config.pi.command, env),
          argv_command: argv_command_with_env(config.pi.argv_command, env),
        ),
      )
  }
}

pub fn retry_state_allowed(
  config: EffectiveConfig,
  state: issue_state.IssueState,
) -> Bool {
  issue_state.contains_normalized(config.tracker.active_states, state)
  || retry_handoff_state_allowed(config, state)
}

pub fn retry_non_retryable_state_reason(
  state: issue_state.IssueState,
) -> String {
  "retry_non_retryable_state:" <> issue_state.to_string(state)
}

pub fn recovery_non_retryable_reason(state: issue_state.IssueState) -> String {
  "recovery_non_retryable_state:" <> issue_state.to_string(state)
}

fn retry_handoff_state_allowed(
  config: EffectiveConfig,
  state: issue_state.IssueState,
) -> Bool {
  retry_handoff_states(config)
  |> list.any(fn(candidate) { issue_state.equals_normalized(candidate, state) })
}

fn retry_handoff_states(
  config: EffectiveConfig,
) -> List(issue_state.IssueState) {
  list.append(
    completion_retry_handoff_states(config),
    legacy_failure_handoff_states(config),
  )
}

fn completion_retry_handoff_states(
  config: EffectiveConfig,
) -> List(issue_state.IssueState) {
  case config.handoff.completion_states {
    None -> []
    Some(policy) -> {
      let global_refs = [policy.failure_state, policy.partial_success_state]
      let global_refs =
        prepend_optional_ref(global_refs, policy.cancellation_state)
      let workflow_refs =
        policy.workflows
        |> dict.values
        |> list.fold([], fn(acc, override) {
          let acc = prepend_optional_ref(acc, override.failure_state)
          let acc = prepend_optional_ref(acc, override.partial_success_state)
          prepend_optional_ref(acc, override.cancellation_state)
        })
      list.append(global_refs, workflow_refs)
      |> list.filter_map(state_ref_to_issue_state)
    }
  }
}

fn legacy_failure_handoff_states(
  config: EffectiveConfig,
) -> List(issue_state.IssueState) {
  case config.handoff.failure_state_id {
    None -> []
    Some(_) ->
      case dict.get(config.linear_contract.handoff_state_bindings, "failure") {
        Error(_) -> []
        Ok(required_state_key) ->
          case
            dict.get(config.linear_contract.required_states, required_state_key)
          {
            Error(_) -> []
            Ok(state_name) -> [issue_state.from_string_unchecked(state_name)]
          }
      }
  }
}

fn prepend_optional_ref(
  refs: List(workflow_completion_policy.LinearStateRef),
  maybe_ref: Option(workflow_completion_policy.LinearStateRef),
) -> List(workflow_completion_policy.LinearStateRef) {
  case maybe_ref {
    None -> refs
    Some(ref) -> [ref, ..refs]
  }
}

fn state_ref_to_issue_state(
  ref: workflow_completion_policy.LinearStateRef,
) -> Result(issue_state.IssueState, Nil) {
  case ref {
    workflow_completion_policy.StateByName(name) ->
      Ok(issue_state.from_string_unchecked(name))
    workflow_completion_policy.StateById(_) -> Error(Nil)
  }
}

fn shell_command_with_env(
  command: String,
  env: List(#(String, String)),
) -> String {
  case env {
    [] -> command
    _ -> shell_export_lines(env) <> command
  }
}

fn shell_export_lines(env: List(#(String, String))) -> String {
  case env {
    [] -> ""
    [#(key, value), ..rest] ->
      "export "
      <> key
      <> "="
      <> shell_quote(value)
      <> "\n"
      <> shell_export_lines(rest)
  }
}

fn shell_quote(value: String) -> String {
  "'" <> string.replace(value, each: "'", with: "'\\''") <> "'"
}

fn argv_command_with_env(
  command: Option(PiArgvCommand),
  env: List(#(String, String)),
) -> Option(PiArgvCommand) {
  case command {
    None -> None
    Some(command) ->
      Some(PiArgvCommand(..command, env: merge_env(command.env, env)))
  }
}

fn merge_env(
  base: List(#(String, String)),
  override: List(#(String, String)),
) -> List(#(String, String)) {
  list.append(remove_env_keys(base, env_keys(override)), override)
}

fn remove_env_keys(
  env: List(#(String, String)),
  keys: List(String),
) -> List(#(String, String)) {
  case env {
    [] -> []
    [#(key, _) as entry, ..rest] ->
      case list.contains(keys, key) {
        True -> remove_env_keys(rest, keys)
        False -> [entry, ..remove_env_keys(rest, keys)]
      }
  }
}

fn env_keys(env: List(#(String, String))) -> List(String) {
  case env {
    [] -> []
    [#(key, _), ..rest] -> [key, ..env_keys(rest)]
  }
}
