import gleam/dict.{type Dict}
import gleam/option.{type Option}
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
