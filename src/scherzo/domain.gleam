import birl.{type Time}
import gleam/dict.{type Dict}
import gleam/option.{type Option}
import scherzo/model_config
import scherzo/orchestrator/reason
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state

pub type BlockerRef {
  BlockerRef(
    id: Option(String),
    identifier: Option(String),
    state: Option(issue_state.IssueState),
  )
}

pub type Issue {
  Issue(
    id: String,
    identifier: String,
    title: String,
    description: Option(String),
    priority: Option(Int),
    state: issue_state.IssueState,
    branch_name: Option(String),
    url: Option(String),
    labels: List(String),
    blocked_by: List(BlockerRef),
    created_at: Option(Time),
    updated_at: Option(Time),
  )
}

pub type TrackerConfig {
  TrackerConfig(
    kind: tracker_kind.TrackerKind,
    endpoint: String,
    api_key: Option(String),
    project_slug: Option(String),
    active_states: List(issue_state.IssueState),
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
  )
}

pub type HandoffConfig {
  HandoffConfig(
    enabled: Bool,
    comment_on_claim: Bool,
    comment_on_success: Bool,
    comment_on_failure: Bool,
    claim_state_id: Option(String),
    success_state_id: Option(String),
    failure_state_id: Option(String),
    include_result_on_success: Bool,
    result_max_chars: Int,
  )
}

pub type ResultArtifact {
  ResultArtifact(
    final_response: Option(String),
    truncated: Bool,
    source: String,
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

pub type ArtifactLimits {
  ArtifactLimits(
    command_stream_max_chars: Int,
    template_field_max_chars: Int,
    workflow_summary_max_chars: Int,
  )
}

pub type OrchestratorConfig {
  OrchestratorConfig(
    effective: EffectiveConfig,
    config_dir: String,
    routing: RoutingConfig,
    dag_hooks: DagHooksConfig,
    artifact_limits: ArtifactLimits,
    model_settings: model_config.Settings,
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

pub type WorkspaceRecord {
  WorkspaceRecord(issue_id: String, identifier: String, path: String)
}

pub type RunAttempt {
  RunAttempt(issue: Issue, attempt: Option(Int), workspace_path: String)
}

pub type TokenTotals {
  TokenTotals(
    input: Int,
    output: Int,
    cache_read: Int,
    cache_write: Int,
    total: Int,
  )
}

pub fn zero_token_totals() -> TokenTotals {
  TokenTotals(input: 0, output: 0, cache_read: 0, cache_write: 0, total: 0)
}

pub type LiveSession {
  LiveSession(
    session_id: String,
    pi_rpc_pid: String,
    last_pi_event: Option(String),
    last_pi_timestamp: Option(Int),
    last_pi_message: Option(String),
    pi_input_tokens: Int,
    pi_output_tokens: Int,
    pi_total_tokens: Int,
    last_reported_input_tokens: Int,
    last_reported_output_tokens: Int,
    last_reported_total_tokens: Int,
    turn_count: Int,
  )
}

pub type RetryEntry {
  RetryEntry(issue_id: String, delay_ms: Int, timer_generation: Int)
}

pub type RunningEntry {
  RunningEntry(
    issue: Issue,
    workspace_path: String,
    session: Option(LiveSession),
  )
}

pub type IssueCounter {
  IssueCounter(failure_attempts: Int, worker_sessions: Int)
}

pub fn new_issue_counter() -> IssueCounter {
  IssueCounter(failure_attempts: 0, worker_sessions: 0)
}

pub type ParkReleasePolicy {
  ExplicitUnparkOnly
  AutoUnparkOnIssueChange(issue_fingerprint: String)
}

pub type ParkedEntry {
  ParkedEntry(
    issue_id: String,
    identifier: String,
    reason: reason.ParkReason,
    release_policy: ParkReleasePolicy,
    parked_at_ms: Int,
  )
}

pub type InvalidWorkflowReport {
  InvalidWorkflowReport(
    issue_id: String,
    identifier: String,
    violation_code: String,
    violation_fingerprint: String,
    reporting_policy_fingerprint: String,
    observed_updated_at: Option(Time),
    observed_labels_fingerprint: String,
    attempted_at_ms: Int,
    last_result: String,
  )
}

pub type RuntimeState {
  RuntimeState(
    poll_interval_ms: Int,
    max_concurrent_agents: Int,
    running: Dict(String, RunningEntry),
    claimed: Dict(String, String),
    retry_attempts: Dict(String, RetryEntry),
    issue_counters: Dict(String, IssueCounter),
    parked: Dict(String, ParkedEntry),
    invalid_workflow_reports: Dict(String, InvalidWorkflowReport),
    completed: Dict(String, Issue),
    aggregate_pi_totals: TokenTotals,
    latest_rate_limit_payload: Option(String),
  )
}
