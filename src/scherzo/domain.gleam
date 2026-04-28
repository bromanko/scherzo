import birl.{type Time}
import gleam/dict.{type Dict}
import gleam/option.{type Option, None}
import yay

pub type BlockerRef {
  BlockerRef(
    id: Option(String),
    identifier: Option(String),
    state: Option(String),
  )
}

pub type Issue {
  Issue(
    id: String,
    identifier: String,
    title: String,
    description: Option(String),
    priority: Option(Int),
    state: String,
    branch_name: Option(String),
    url: Option(String),
    labels: List(String),
    blocked_by: List(BlockerRef),
    created_at: Option(Time),
    updated_at: Option(Time),
  )
}

pub type WorkflowDefinition {
  WorkflowDefinition(config: yay.Node, prompt_template: String)
}

pub type TrackerConfig {
  TrackerConfig(
    kind: String,
    endpoint: String,
    api_key: Option(String),
    project_slug: Option(String),
    active_states: List(String),
    terminal_states: List(String),
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
}

pub type AgentConfig {
  AgentConfig(
    max_concurrent_agents: Int,
    max_turns: Int,
    max_retry_backoff_ms: Int,
    max_retry_attempts: Int,
    max_sessions_per_issue: Int,
    max_concurrent_agents_by_state: Dict(String, Int),
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
  RetryEntry(issue_id: String, due_at_ms: Int, timer_generation: Int)
}

pub type RunningEntry {
  RunningEntry(
    issue: Issue,
    workspace_path: String,
    session: Option(LiveSession),
  )
}

pub type IssueCounter {
  IssueCounter(
    failure_attempts: Int,
    worker_sessions: Int,
    observed_updated_at: Option(Time),
  )
}

pub fn new_issue_counter() -> IssueCounter {
  IssueCounter(
    failure_attempts: 0,
    worker_sessions: 0,
    observed_updated_at: None,
  )
}

pub type ParkedEntry {
  ParkedEntry(
    issue_id: String,
    identifier: String,
    reason: String,
    observed_updated_at: Option(Time),
    parked_at_ms: Int,
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
    completed: Dict(String, Issue),
    aggregate_pi_totals: TokenTotals,
    latest_rate_limit_payload: Option(String),
  )
}
