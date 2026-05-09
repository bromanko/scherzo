import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/string
import scherzo/model_config
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state

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

pub type HandoffConfig {
  HandoffConfig(
    enabled: Bool,
    comment_on_claim: Bool,
    comment_on_success: Bool,
    comment_on_failure: Bool,
    comment_on_park: Bool,
    claim_state_id: Option(String),
    success_state_id: Option(String),
    failure_state_id: Option(String),
    include_result_on_success: Bool,
    attach_result_on_success: Bool,
    attachment_fallback_to_markdown_link: Bool,
    result_max_chars: Int,
  )
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
    comment_on_invalid_workflow: Bool,
  )
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
      "linear_contract.workflow_label_prefix must match routing.workflow_label_prefix"
    LinearContractRoutingWorkflowLabelsMismatch ->
      "linear_contract.workflow_labels must match issue-dispatched routing.workflows when routing requires exactly one workflow label"
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
  case require_exactly_one_workflow_label, has_labels {
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

pub type WorkspaceProfileSource {
  LegacyWorkspaceHooks
  ConfiguredWorkspaceProfile
}

pub type WorkspaceHookProfile {
  WorkspaceHookProfile(
    name: String,
    hooks: DagHooksConfig,
    source: WorkspaceProfileSource,
  )
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
  OpenIssuePerJob
}

pub type ScheduledLinearFailureConfig {
  ScheduledLinearFailureConfig(
    enabled: Bool,
    state: Option(String),
    labels: List(String),
    dedupe: ScheduledFailureDedupe,
  )
}

pub type ScheduledFailureConfig {
  ScheduledFailureConfig(linear: ScheduledLinearFailureConfig)
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
  )
}
