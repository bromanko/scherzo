import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import orchestrator_transition_invariant_helpers as invariant_helpers
import scherzo/artifact_publication_config
import scherzo/config
import scherzo/config/types as config_types
import scherzo/model_config
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/task_lifecycle
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/workflow_snapshot
import scherzo/review_lane_preflight_policy
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger
import scherzo/task
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag

pub fn fixture_issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-1",
    identifier: "ABC-1",
    title: "Implement transition kernel",
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

pub fn fixture_runtime() -> orchestrator_state.RuntimeState {
  orchestrator_state.RuntimeState(
    poll_interval_ms: 30_000,
    max_concurrent_agents: 2,
    running: dict.new(),
    claimed: dict.new(),
    retry_attempts: dict.new(),
    issue_counters: dict.new(),
    parked: dict.new(),
    invalid_workflow_reports: dict.new(),
    blocked_dependency_reports: dict.new(),
    completed: dict.new(),
    completed_at_ms: dict.new(),
    aggregate_pi_totals: session_tokens.zero_token_totals(),
    latest_rate_limit_payload: None,
  )
}

pub fn fixture_effective() -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config.default_tracker_config(),
    polling: config.default_polling_config(),
    workspace: config_types.WorkspaceConfig(root: "test/tmp/workspaces"),
    hooks: config.default_hooks_config(),
    agent: config_types.AgentConfig(
      ..config.default_agent_config(),
      max_concurrent_agents: 2,
    ),
    pi: config.default_pi_config(),
    handoff: config.default_handoff_config(),
    linear_contract: config.default_linear_contract_config(),
    linear_commands: config.default_linear_command_config(),
    ui_server: config.default_ui_server_config(),
  )
}

pub fn fixture_routing() -> config_types.RoutingConfig {
  config_types.RoutingConfig(
    workflow_label_prefix: "workflow:",
    require_exactly_one_workflow_label: False,
    default_workflow: Some("default"),
    workflows: dict.from_list([#("default", "workflows/default.yaml")]),
  )
}

pub fn fixture_orchestrator() -> config_types.OrchestratorConfig {
  config_types.OrchestratorConfig(
    effective: fixture_effective(),
    config_dir: ".scherzo",
    routing: fixture_routing(),
    dag_hooks: config_types.empty_dag_hooks(),
    workspace_profiles: config_types.WorkspaceHookProfiles(
      default_profile: "default",
      profiles: dict.from_list([
        #(
          "default",
          config_types.WorkspaceHookProfile(
            name: "default",
            driver: None,
            source: config_types.SyntheticDefaultWorkspace,
          ),
        ),
      ]),
    ),
    artifact_limits: config_types.ArtifactLimits(
      command_stream_max_chars: 20_000,
      template_field_max_chars: 8000,
      workflow_summary_max_chars: 20_000,
    ),
    artifact_repositories: artifact_publication_config.empty_repositories(),
    model_settings: model_config.default_settings(),
    scheduled_jobs: [],
  )
}

pub fn fixture_context() -> transition_types.DispatchContext {
  transition_types.DispatchContext(
    effective: fixture_effective(),
    orchestrator: fixture_orchestrator(),
    tracker_backend_kind: "linear",
    routing: fixture_routing(),
    available_workflow_ids: ["default"],
    dispatch_enabled: True,
    operator_paused: False,
    active_issue_ids: [],
    active_issues: [],
    reserved_non_issue_slots: 0,
    workspace_root: "test/tmp/workspaces",
    now_ms: 123,
    recovery_by_issue: dict.new(),
    review_lane_preflight: transition_types.ReviewLanePreflightContext(
      config_dir: ".scherzo",
      workflow_dags: dict.from_list([
        #("default", fixture_workflow_dag("default")),
      ]),
      policy: review_lane_preflight_policy.Policy(
        mode: review_lane_preflight_policy.Off,
        cache_ttl_seconds: 86_400,
        park_on_failure: True,
        strict_live_model_checks: False,
      ),
      override: None,
    ),
  )
}

pub fn fixture_workflow_snapshot(
  workflow_id: String,
  issue: tracker_issue.Issue,
  _run_id: String,
) -> workflow_snapshot.Snapshot {
  workflow_snapshot.Snapshot(
    workflow_id: workflow_id,
    dag: fixture_workflow_dag(workflow_id),
    orchestrator: fixture_orchestrator(),
    fingerprint: "workflow-fingerprint",
    run_root: "test/tmp/workspaces/" <> issue.identifier,
  )
}

pub fn fixture_workflow_dag(id: String) -> workflow_dag.WorkflowDag {
  workflow_dag.WorkflowDag(
    id: id,
    description: None,
    workspace_profile: None,
    workspace_capabilities: [],
    max_parallel_steps: 1,
    recover: None,
    contract: None,
    publication_routes: [],
    workstream_phase: None,
    steps: [
      workflow_dag.WorkflowStep(
        id: "noop",
        kind: workflow_dag.CommandStep(run: "true", timeout_ms: None),
        depends_on: [],
        workspace: workflow_dag.WorkspaceRef(name: "default", from: None),
        on_failure: workflow_dag.FailWorkflow,
        model_settings: model_config.default_settings(),
        recover: None,
      ),
    ],
  )
}

pub fn fixture_state() -> transition_types.State {
  transition_types.State(
    runtime: fixture_runtime(),
    workers: transition_types.new_worker_directory(),
    pending_claims: dict.new(),
    pending_dispatch_validations: dict.new(),
    pending_review_lane_preflights: dict.new(),
    lifecycle: task_lifecycle.new(),
    retry_refresh_generations: dict.new(),
    next_dispatch_validation_generation: 1,
    next_session_sequence: 1,
  )
}

pub fn state_with_pending_claim(
  issue: tracker_issue.Issue,
) -> transition_types.State {
  let state = fixture_state()
  let task_ref = task.from_legacy_issue(issue).ref
  transition_types.State(
    ..state,
    pending_claims: dict.insert(
      state.pending_claims,
      orchestrator_state.task_ref_identity(task_ref),
      transition_types.PendingClaim(
        task_ref: task_ref,
        issue_id: issue.id,
        run_id: "run-1",
        session_id: "session-1",
        workspace_path: "test/tmp/workspaces/ABC-1",
        workflow_id: "default",
        workflow_snapshot: fixture_workflow_snapshot("default", issue, "run-1"),
        command_route_id: "worker:run-1:1",
        route_label: issue.identifier,
        issue: issue,
        recovery: None,
        remaining_candidates: [],
        dispatch_context: fixture_context(),
        previous_retry_generation: 0,
        retry_cancellation: None,
      ),
    ),
    lifecycle: {
      let assert Ok(directory) =
        task_lifecycle.put(
          task_lifecycle.new(),
          task_lifecycle.Claiming(
            task_ref: task_ref,
            issue: issue,
            run_id: "run-1",
            session_id: "session-1",
          ),
        )
      directory
    },
  )
}

pub fn snapshot_returns_runtime_state_test() {
  let state = fixture_state()

  assert transition.snapshot(state) == state.runtime
}

pub fn retry_refresh_failure_logs_error_before_reschedule_test() {
  let issue = fixture_issue()
  let runtime = fixture_runtime()
  let runtime =
    orchestrator_state.RuntimeState(
      ..runtime,
      retry_attempts: dict.insert(
        runtime.retry_attempts,
        orchestrator_state.issue_identity(issue),
        orchestrator_state.RetryEntry(
          task_ref: task.from_legacy_issue(issue).ref,
          issue_id: issue.id,
          delay_ms: 1000,
          timer_generation: 7,
        ),
      ),
      claimed: dict.insert(
        runtime.claimed,
        orchestrator_state.issue_identity(issue),
        issue.identifier,
      ),
    )
  let state = transition_types.State(..fixture_state(), runtime: runtime)

  let outcome =
    invariant_helpers.handle_and_assert(
      transition_types.RetryRefreshCompleted(
        issue.id,
        7,
        Error("tracker_unavailable"),
        fixture_context(),
      ),
      state,
    )

  assert list.any(outcome.effects, fn(effect) {
    case effect {
      effects_types.Log(
        "warn",
        "retry_refresh_failed",
        [#("issue_id", "issue-1"), #("error", "tracker_unavailable")],
      ) -> True
      _ -> False
    }
  })
  assert list.any(outcome.effects, fn(effect) {
    case effect {
      effects_types.AppendLedger(effects_types.LedgerAppend(
        policy: effects_types.ScheduleRetryTimerAfterAppend(
          issue_id: "issue-1",
          retry_reason: orchestrator_reason.RetryPollFailed,
          ..,
        ),
        ..,
      )) -> True
      _ -> False
    }
  })
}

pub fn schedule_retry_append_failure_without_previous_retry_does_not_leave_timer_test() {
  let issue = fixture_issue()
  let runtime =
    orchestrator_state.RuntimeState(
      ..fixture_runtime(),
      retry_attempts: dict.from_list([
        #(
          orchestrator_state.issue_identity(issue),
          orchestrator_state.RetryEntry(
            task_ref: task.from_legacy_issue(issue).ref,
            issue_id: issue.id,
            delay_ms: 10_000,
            timer_generation: 1,
          ),
        ),
      ]),
      claimed: dict.from_list([
        #(orchestrator_state.issue_identity(issue), issue.identifier),
      ]),
    )
  let state = transition_types.State(..fixture_state(), runtime: runtime)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "retry_schedule:issue-1:1",
        continuation: transition_types.ScheduleRetryTimerAfterAppend(
          issue_id: "issue-1",
          delay_ms: 10_000,
          generation: 1,
          retry_reason: orchestrator_reason.RetryPollFailed,
          previous_retry: None,
        ),
        result: Error(ledger.Io("disk full")),
        now_ms: 123,
      ),
      state,
    )

  assert dict.get(
      next.runtime.retry_attempts,
      orchestrator_state.issue_identity(issue),
    )
    == Error(Nil)
  assert dict.get(
      next.runtime.claimed,
      orchestrator_state.issue_identity(issue),
    )
    == Error(Nil)
  assert !list.any(effects, fn(effect) {
    case effect {
      effects_types.ScheduleRetryTimer(_, _, _, _) -> True
      effects_types.DeferRetryTimer(_, _, _) -> True
      _ -> False
    }
  })
  assert list.any(effects, fn(effect) {
    effect
    == effects_types.Log("warn", "ledger_append_failed", [
      #("issue_id", "issue-1"),
      #("generation", "1"),
      #("correlation_id", "retry_schedule:issue-1:1"),
      #("error", "io"),
    ])
  })
}

pub fn schedule_retry_append_failure_without_previous_retry_cleans_non_linear_identity_test() {
  let issue = fixture_issue()
  let retry_ref = orchestrator_state.issue_ref_for_backend(issue, "memory")
  let retry_identity = orchestrator_state.task_ref_identity(retry_ref)
  let runtime =
    orchestrator_state.RuntimeState(
      ..fixture_runtime(),
      retry_attempts: dict.from_list([
        #(
          retry_identity,
          orchestrator_state.RetryEntry(
            task_ref: retry_ref,
            issue_id: issue.id,
            delay_ms: 10_000,
            timer_generation: 1,
          ),
        ),
      ]),
      claimed: dict.from_list([#(retry_identity, issue.identifier)]),
    )
  let state = transition_types.State(..fixture_state(), runtime: runtime)

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "retry_schedule:issue-1:1",
        continuation: transition_types.ScheduleRetryTimerAfterAppend(
          issue_id: "issue-1",
          delay_ms: 10_000,
          generation: 1,
          retry_reason: orchestrator_reason.RetryPollFailed,
          previous_retry: None,
        ),
        result: Error(ledger.Io("disk full")),
        now_ms: 123,
      ),
      state,
    )

  assert dict.get(next.runtime.retry_attempts, retry_identity) == Error(Nil)
  assert dict.get(next.runtime.claimed, retry_identity) == Error(Nil)
  assert !list.any(effects, fn(effect) {
    case effect {
      effects_types.ScheduleRetryTimer(_, _, _, _) -> True
      effects_types.DeferRetryTimer(_, _, _) -> True
      _ -> False
    }
  })
}

pub fn cancel_retry_append_success_emits_cancel_timer_test() {
  let transition_types.Outcome(effects: effects, ..) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "retry_cancel:issue-1:3",
        continuation: transition_types.CancelRetryTimerAfterAppend(
          issue_id: "issue-1",
          generation: 3,
          cancel_reason: "operator",
          previous_retry: None,
        ),
        result: Ok(Nil),
        now_ms: 123,
      ),
      fixture_state(),
    )

  assert effects == [effects_types.CancelRetryTimer("issue-1", 3, "operator")]
}

pub fn cancel_retry_append_failure_restores_previous_retry_and_defers_timer_test() {
  let issue = fixture_issue()
  let previous_retry =
    orchestrator_state.RetryEntry(
      task_ref: task.from_legacy_issue(issue).ref,
      issue_id: issue.id,
      delay_ms: 10_000,
      timer_generation: 3,
    )

  let transition_types.Outcome(state: next, effects: effects) =
    transition.handle(
      transition_types.LedgerAppendCompleted(
        correlation_id: "retry_cancel:issue-1:3",
        continuation: transition_types.CancelRetryTimerAfterAppend(
          issue_id: "issue-1",
          generation: 3,
          cancel_reason: "operator",
          previous_retry: Some(previous_retry),
        ),
        result: Error(ledger.Io("disk full")),
        now_ms: 123,
      ),
      fixture_state(),
    )

  let assert Ok(retry) =
    dict.get(
      next.runtime.retry_attempts,
      orchestrator_state.issue_identity(issue),
    )
  assert retry == previous_retry
  assert dict.get(
      next.runtime.claimed,
      orchestrator_state.issue_identity(issue),
    )
    == Ok(issue.identifier)
  assert list.any(effects, fn(effect) {
    effect == effects_types.DeferRetryTimer("issue-1", 3, 10_000)
  })
  assert !list.any(effects, fn(effect) {
    case effect {
      effects_types.CancelRetryTimer(_, _, _) -> True
      _ -> False
    }
  })
}
