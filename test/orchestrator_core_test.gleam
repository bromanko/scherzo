import birl
import gleam/dict
import gleam/option.{type Option, None, Some}
import scherzo/domain
import scherzo/orchestrator/core
import scherzo/workflow_policy

fn config() -> domain.EffectiveConfig {
  domain.EffectiveConfig(
    tracker: domain.TrackerConfig(
      kind: "linear",
      endpoint: "endpoint",
      api_key: Some("key"),
      project_slug: Some("PROJ"),
      active_states: ["Todo", "In Progress"],
      terminal_states: ["Done", "Closed", "Canceled", "Cancelled", "Duplicate"],
    ),
    polling: domain.PollingConfig(interval_ms: 30_000),
    workspace: domain.WorkspaceConfig(root: "test/tmp/workspaces"),
    hooks: domain.HooksConfig(
      after_create: Some("true"),
      before_run: None,
      after_run: None,
      before_remove: None,
      timeout_ms: 1000,
    ),
    agent: domain.AgentConfig(
      max_concurrent_agents: 2,
      max_turns: 20,
      max_retry_backoff_ms: 40_000,
      max_retry_attempts: 3,
      max_sessions_per_issue: 2,
      max_concurrent_agents_by_state: dict.from_list([#("todo", 1)]),
    ),
    pi: domain.PiConfig(
      command: "fake",
      turn_timeout_ms: 1000,
      read_timeout_ms: 1000,
      stall_timeout_ms: 1000,
      auto_retry: True,
      ui_request_policy: domain.Cancel,
      ui_request_timeout_ms: 300_000,
      compatibility_probe: True,
      rate_limit_payload: None,
    ),
    handoff: domain.HandoffConfig(
      enabled: False,
      comment_on_claim: False,
      comment_on_success: False,
      comment_on_failure: False,
      claim_state_id: None,
      success_state_id: None,
      failure_state_id: None,
    ),
    linear_contract: domain.LinearContractConfig(
      enabled: False,
      workflow_label_prefix: "workflow:",
      workflow_labels: [],
      support_labels: [],
      required_states: dict.new(),
      handoff_state_bindings: dict.new(),
      enforce_issue_workflow_labels: False,
      invalid_workflow_state_id: None,
      comment_on_invalid_workflow: False,
    ),
    linear_commands: domain.LinearCommandConfig(
      enabled: False,
      prefix: "/scherzo",
      authorized_user_ids: [],
      poll_limit_per_issue: 25,
      max_comments_per_tick: 50,
      acknowledge_success: True,
      acknowledge_rejection: True,
    ),
  )
}

fn enforcing_config() -> domain.EffectiveConfig {
  domain.EffectiveConfig(
    ..config(),
    linear_contract: domain.LinearContractConfig(
      ..config().linear_contract,
      workflow_labels: ["bugfix", "research"],
      enforce_issue_workflow_labels: True,
    ),
  )
}

fn issue(
  id: String,
  identifier: String,
  state: String,
  priority: Option(Int),
) -> domain.Issue {
  domain.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: priority,
    state: state,
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn rich_issue() -> domain.Issue {
  domain.Issue(
    ..issue("a", "ABC-1", "Todo", Some(1)),
    description: Some("Description"),
    branch_name: Some("abc-1-title"),
    url: Some("https://linear.app/example/ABC-1"),
    labels: ["bug", "backend"],
    blocked_by: [
      domain.BlockerRef(
        id: Some("block-1"),
        identifier: Some("ABC-0"),
        state: Some("Todo"),
      ),
      domain.BlockerRef(
        id: Some("block-2"),
        identifier: Some("ABC-00"),
        state: Some("In Progress"),
      ),
    ],
    created_at: Some(birl.from_unix(1)),
    updated_at: Some(birl.from_unix(2)),
  )
}

fn auto_parked_entry(issue: domain.Issue, reason: String) -> domain.ParkedEntry {
  domain.ParkedEntry(
    issue_id: issue.id,
    identifier: issue.identifier,
    reason: reason,
    release_policy: domain.AutoUnparkOnIssueChange(core.issue_fingerprint(issue)),
    parked_at_ms: 0,
  )
}

fn explicit_parked_entry(
  issue: domain.Issue,
  reason: String,
) -> domain.ParkedEntry {
  domain.ParkedEntry(
    issue_id: issue.id,
    identifier: issue.identifier,
    reason: reason,
    release_policy: domain.ExplicitUnparkOnly,
    parked_at_ms: 0,
  )
}

fn state_with_parked(
  state: domain.RuntimeState,
  issue: domain.Issue,
  parked: domain.ParkedEntry,
) -> domain.RuntimeState {
  domain.RuntimeState(
    ..state,
    parked: dict.insert(state.parked, issue.id, parked),
  )
}

pub fn issue_fingerprint_ignores_timestamps_url_and_labels_test() {
  let base = rich_issue()
  let metadata_changed =
    domain.Issue(
      ..base,
      url: Some("https://linear.app/example/ABC-1?metadata=changed"),
      labels: ["frontend", "needs-review"],
      created_at: Some(birl.from_unix(100)),
      updated_at: Some(birl.from_unix(101)),
    )

  assert core.issue_fingerprint(base)
    == core.issue_fingerprint(metadata_changed)
}

pub fn issue_fingerprint_changes_for_blockers_test() {
  let first =
    domain.BlockerRef(
      id: Some("block-1"),
      identifier: Some("ABC-0"),
      state: Some("Todo"),
    )
  let second =
    domain.BlockerRef(
      id: Some("block-2"),
      identifier: Some("ABC-00"),
      state: Some("In Progress"),
    )
  let base = domain.Issue(..rich_issue(), blocked_by: [first, second])
  let reordered = domain.Issue(..base, blocked_by: [second, first])
  let blocker_changed =
    domain.Issue(..base, blocked_by: [
      domain.BlockerRef(..first, state: Some("Done")),
      second,
    ])

  assert core.issue_fingerprint(base) == core.issue_fingerprint(reordered)
  assert core.issue_fingerprint(base) != core.issue_fingerprint(blocker_changed)
}

pub fn issue_fingerprint_changes_for_core_fields_test() {
  let base = rich_issue()
  let base_fingerprint = core.issue_fingerprint(base)

  assert core.issue_fingerprint(domain.Issue(..base, id: "different-id"))
    != base_fingerprint
  assert core.issue_fingerprint(domain.Issue(..base, identifier: "ABC-2"))
    != base_fingerprint
  assert core.issue_fingerprint(domain.Issue(..base, title: "Different title"))
    != base_fingerprint
  assert core.issue_fingerprint(domain.Issue(..base, description: Some("New")))
    != base_fingerprint
  assert core.issue_fingerprint(domain.Issue(..base, priority: Some(2)))
    != base_fingerprint
  assert core.issue_fingerprint(domain.Issue(..base, state: "In Progress"))
    != base_fingerprint
  assert core.issue_fingerprint(domain.Issue(..base, branch_name: Some("new")))
    != base_fingerprint
}

pub fn candidate_sorting_and_eligibility_test() {
  let a = issue("a", "ABC-2", "Todo", None)
  let b = issue("b", "ABC-1", "Todo", Some(1))
  assert core.sort_candidates([a, b]) == [b, a]

  let state = core.new_state(config())
  assert core.should_dispatch(state, config(), b)
  assert !core.should_dispatch(state, config(), domain.Issue(..b, id: ""))
  assert !core.should_dispatch(
    state,
    config(),
    domain.Issue(..b, state: "Done"),
  )
}

pub fn workflow_policy_rejects_invalid_dispatch_test() {
  let base = issue("a", "ABC-1", "Todo", Some(1))
  let state = core.new_state(enforcing_config())
  assert core.should_dispatch(
    state,
    enforcing_config(),
    domain.Issue(..base, labels: ["workflow:bugfix"]),
  )
  assert !core.should_dispatch(state, enforcing_config(), base)
  assert !core.should_dispatch(
    state,
    enforcing_config(),
    domain.Issue(..base, labels: ["workflow:bugfix", "workflow:research"]),
  )
  assert !core.should_dispatch(
    state,
    enforcing_config(),
    domain.Issue(..base, labels: ["workflow:unknown"]),
  )
  assert core.should_dispatch(state, config(), base)
}

pub fn dispatch_preconditions_skip_parked_before_workflow_reporting_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let parked =
    state_with_parked(
      core.new_state(enforcing_config()),
      issue,
      explicit_parked_entry(issue, "operator_hold"),
    )
  assert !core.dispatch_preconditions_satisfied(
    parked,
    enforcing_config(),
    issue,
  )
  assert !core.should_dispatch(parked, enforcing_config(), issue)
}

pub fn retry_policy_invalid_can_stop_retry_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let running =
    core.apply_worker_start(
      core.new_state(enforcing_config()),
      issue,
      "/tmp/ws",
    )
  let core.Transition(state: retry_state, effects: _) =
    core.apply_worker_failure(running, enforcing_config(), "a", issue, 100)
  assert core.retry_candidate_preconditions_satisfied(
    retry_state,
    enforcing_config(),
    "a",
    issue,
  )
  let core.Transition(state: stopped, effects:) =
    core.stop_retry_for_policy_invalid(retry_state, "a")
  assert !dict.has_key(stopped.retry_attempts, "a")
  assert !dict.has_key(stopped.claimed, "a")
  assert effects == [core.CancelRetry("a"), core.ReleaseClaim("a")]
}

pub fn invalid_workflow_report_fingerprint_helpers_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let violation = workflow_policy.MissingWorkflowLabel
  let pending =
    core.mark_invalid_workflow_report_pending(
      core.new_state(enforcing_config()),
      issue,
      violation,
      enforcing_config().linear_contract,
      123,
    )
  assert core.already_attempted_invalid_workflow(
    pending,
    issue,
    violation,
    enforcing_config().linear_contract,
  )
  let changed =
    domain.Issue(..issue, labels: ["workflow:unknown"], updated_at: None)
  assert !core.already_attempted_invalid_workflow(
    pending,
    changed,
    workflow_policy.UnknownWorkflowLabel("workflow:unknown"),
    enforcing_config().linear_contract,
  )
  let unknown_issue =
    domain.Issue(..issue, labels: ["workflow:surprise"], updated_at: None)
  let unknown_violation =
    workflow_policy.UnknownWorkflowLabel("workflow:surprise")
  let unknown_pending =
    core.mark_invalid_workflow_report_pending(
      core.new_state(enforcing_config()),
      unknown_issue,
      unknown_violation,
      enforcing_config().linear_contract,
      124,
    )
  assert !core.already_attempted_invalid_workflow(
    unknown_pending,
    domain.Issue(..unknown_issue, labels: ["workflow:other"]),
    workflow_policy.UnknownWorkflowLabel("workflow:other"),
    enforcing_config().linear_contract,
  )

  let changed_policy =
    domain.LinearContractConfig(
      ..enforcing_config().linear_contract,
      comment_on_invalid_workflow: True,
    )
  assert !core.already_attempted_invalid_workflow(
    pending,
    issue,
    violation,
    changed_policy,
  )

  let updated =
    core.mark_invalid_workflow_report_result(
      pending,
      issue.id,
      workflow_policy.violation_fingerprint(violation),
      workflow_policy.reporting_policy_fingerprint(
        enforcing_config().linear_contract,
      ),
      "noop",
    )
  let assert Ok(report) = dict.get(updated.invalid_workflow_reports, issue.id)
  assert report.last_result == "noop"

  let failed =
    core.mark_invalid_workflow_report_result(
      pending,
      issue.id,
      workflow_policy.violation_fingerprint(violation),
      workflow_policy.reporting_policy_fingerprint(
        enforcing_config().linear_contract,
      ),
      "failed",
    )
  assert !core.already_attempted_invalid_workflow(
    failed,
    issue,
    violation,
    enforcing_config().linear_contract,
  )

  let cleared = core.clear_invalid_workflow_report(updated, issue.id)
  assert !dict.has_key(cleared.invalid_workflow_reports, issue.id)
}

pub fn running_claimed_parked_and_slots_reject_dispatch_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state = core.new_state(config())
  let running = core.apply_worker_start(state, issue, "/tmp/ws")
  assert !core.should_dispatch(running, config(), issue)

  let claimed =
    domain.RuntimeState(..state, claimed: dict.from_list([#("a", "ABC-1")]))
  assert !core.should_dispatch(claimed, config(), issue)

  let parked =
    state_with_parked(
      state,
      issue,
      explicit_parked_entry(issue, "operator_hold"),
    )
  assert !core.should_dispatch(parked, config(), issue)

  let paused =
    domain.EffectiveConfig(
      ..config(),
      agent: domain.AgentConfig(..config().agent, max_concurrent_agents: 0),
    )
  assert !core.should_dispatch(state, paused, issue)
}

pub fn per_state_slots_and_blockers_test() {
  let first = issue("a", "ABC-1", "Todo", Some(1))
  let second = issue("b", "ABC-2", "Todo", Some(2))
  let state =
    core.apply_worker_start(core.new_state(config()), first, "/tmp/ws")
  assert !core.should_dispatch(state, config(), second)

  let blocker =
    domain.BlockerRef(
      id: Some("block"),
      identifier: Some("B-1"),
      state: Some("Todo"),
    )
  assert !core.should_dispatch(
    core.new_state(config()),
    config(),
    domain.Issue(..second, blocked_by: [blocker]),
  )
  let done_blocker = domain.BlockerRef(..blocker, state: Some("Done"))
  assert core.should_dispatch(
    core.new_state(config()),
    config(),
    domain.Issue(..second, blocked_by: [done_blocker]),
  )
}

pub fn worker_success_terminal_cleans_and_releases_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let final = domain.Issue(..issue, state: "Done")
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: next, effects:) =
    core.apply_worker_success_with_workspace_path(
      state,
      config(),
      "a",
      final,
      "/tmp/ws",
      domain.zero_token_totals(),
      100,
    )
  assert dict.has_key(next.running, "a") == False
  assert dict.has_key(next.claimed, "a") == False
  assert effects == [core.CleanupWorkspace("/tmp/ws"), core.ReleaseClaim("a")]
}

pub fn worker_success_active_schedules_continuation_then_parks_at_cap_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: next, effects:) =
    core.apply_worker_success_with_workspace_path(
      state,
      config(),
      "a",
      issue,
      "/tmp/ws",
      domain.zero_token_totals(),
      100,
    )
  assert effects
    == [core.CancelRetry("a"), core.ScheduleRetry("a", 1000, 1, "continuation")]

  let state2 = core.apply_worker_start(next, issue, "/tmp/ws")
  let core.Transition(state: parked, effects: park_effects) =
    core.apply_worker_success_with_workspace_path(
      state2,
      config(),
      "a",
      issue,
      "/tmp/ws",
      domain.zero_token_totals(),
      200,
    )
  assert dict.has_key(parked.parked, "a")
  let assert Ok(parked_entry) = dict.get(parked.parked, "a")
  assert parked_entry.release_policy
    == domain.AutoUnparkOnIssueChange(core.issue_fingerprint(issue))
  assert park_effects
    == [core.ParkIssue("a", "max_sessions_per_issue"), core.ReleaseClaim("a")]
}

pub fn worker_failure_backoff_and_retry_cap_test() {
  assert core.backoff_delay(1, 40_000) == 10_000
  assert core.backoff_delay(2, 40_000) == 20_000
  assert core.backoff_delay(3, 40_000) == 40_000

  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: one, effects: effects1) =
    core.apply_worker_failure(state, config(), "a", issue, 100)
  assert effects1
    == [core.CancelRetry("a"), core.ScheduleRetry("a", 10_000, 1, "failure")]
  let state = core.apply_worker_start(one, issue, "/tmp/ws")
  let core.Transition(state: two, effects: _) =
    core.apply_worker_failure(state, config(), "a", issue, 200)
  let latest = domain.Issue(..issue, title: "Latest failure title")
  let state = core.apply_worker_start(two, issue, "/tmp/ws")
  let core.Transition(state: parked, effects: effects3) =
    core.apply_worker_failure(state, config(), "a", latest, 300)
  assert dict.has_key(parked.parked, "a")
  let assert Ok(parked_entry) = dict.get(parked.parked, "a")
  assert parked_entry.release_policy
    == domain.AutoUnparkOnIssueChange(core.issue_fingerprint(latest))
  assert parked_entry.release_policy
    != domain.AutoUnparkOnIssueChange(core.issue_fingerprint(issue))
  assert effects3
    == [core.ParkIssue("a", "max_retry_attempts"), core.ReleaseClaim("a")]
}

pub fn worker_failure_uses_dispatched_issue_id_for_lifecycle_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let mismatched_final_issue =
    domain.Issue(
      ..issue,
      id: "different-id",
      identifier: "ABC-999",
      title: "Different issue",
    )
  let retry_cap_config =
    domain.EffectiveConfig(
      ..config(),
      agent: domain.AgentConfig(..config().agent, max_retry_attempts: 1),
    )
  let state =
    core.apply_worker_start(core.new_state(retry_cap_config), issue, "/tmp/ws")

  let core.Transition(state: parked, effects:) =
    core.apply_worker_failure(
      state,
      retry_cap_config,
      "a",
      mismatched_final_issue,
      100,
    )

  assert !dict.has_key(parked.running, "a")
  assert !dict.has_key(parked.running, "different-id")
  assert dict.has_key(parked.parked, "a")
  assert !dict.has_key(parked.parked, "different-id")
  assert effects
    == [core.ParkIssue("a", "max_retry_attempts"), core.ReleaseClaim("a")]
}

pub fn retry_candidate_can_dispatch_self_claimed_issue_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: retry_state, effects: _) =
    core.apply_worker_failure(state, config(), "a", issue, 100)

  let updated = domain.Issue(..issue, title: "Updated title")
  let core.Transition(state: next, effects: effects) =
    core.handle_retry_candidate(retry_state, config(), "a", Ok(Some(updated)))

  assert effects == [core.Dispatch(updated)]
  assert !dict.has_key(next.retry_attempts, "a")
  assert dict.has_key(next.claimed, "a")
}

pub fn continuation_retry_can_dispatch_self_claimed_issue_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: retry_state, effects: _) =
    core.apply_worker_success_with_workspace_path(
      state,
      config(),
      "a",
      issue,
      "/tmp/ws",
      domain.zero_token_totals(),
      100,
    )

  let core.Transition(state: next, effects: effects) =
    core.handle_retry_candidate(retry_state, config(), "a", Ok(Some(issue)))

  assert effects == [core.Dispatch(issue)]
  assert !dict.has_key(next.retry_attempts, "a")
  assert dict.has_key(next.claimed, "a")
}

pub fn retry_timer_handling_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: retry_state, effects: _) =
    core.apply_worker_failure(state, config(), "a", issue, 100)
  let core.Transition(effects: poll_failed, state: kept) =
    core.handle_retry_candidate(retry_state, config(), "a", Error("tracker"))
  assert dict.has_key(kept.claimed, "a")
  assert poll_failed
    == [
      core.CancelRetry("a"),
      core.ScheduleRetry("a", 1000, 2, "retry poll failed"),
    ]

  let core.Transition(effects: absent, state: released) =
    core.handle_retry_candidate(kept, config(), "a", Ok(None))
  assert absent == [core.ReleaseClaim("a")]
  assert !dict.has_key(released.claimed, "a")
}

pub fn explicit_park_blocks_even_when_issue_changes_test() {
  let issue = rich_issue()
  let state =
    state_with_parked(
      core.new_state(config()),
      issue,
      explicit_parked_entry(issue, "operator_abort"),
    )
  let changed =
    domain.Issue(
      ..issue,
      title: "Changed title",
      description: Some("Changed description"),
      updated_at: Some(birl.from_unix(999)),
    )

  let kept = core.unpark_if_issue_changed(state, changed)
  assert dict.has_key(kept.parked, issue.id)
  assert !core.should_dispatch(kept, config(), changed)
}

pub fn auto_park_ignores_comment_and_non_core_changes_test() {
  let issue = rich_issue()
  let state =
    state_with_parked(
      core.new_state(config()),
      issue,
      auto_parked_entry(issue, "max_retry_attempts"),
    )

  let timestamp_changed =
    domain.Issue(..issue, updated_at: Some(birl.from_unix(200)))
  assert_auto_park_blocks(state, timestamp_changed)

  let url_changed = domain.Issue(..issue, url: Some("https://example.invalid"))
  assert_auto_park_blocks(state, url_changed)

  let labels_changed = domain.Issue(..issue, labels: ["new", "metadata"])
  assert_auto_park_blocks(state, labels_changed)
}

pub fn auto_park_clears_on_blocker_change_test() {
  let issue = rich_issue()
  let state =
    state_with_parked(
      core.new_state(config()),
      issue,
      auto_parked_entry(issue, "max_retry_attempts"),
    )
  let blockers_satisfied =
    domain.Issue(..issue, blocked_by: [
      domain.BlockerRef(
        id: Some("block-1"),
        identifier: Some("ABC-0"),
        state: Some("Done"),
      ),
      domain.BlockerRef(
        id: Some("block-2"),
        identifier: Some("ABC-00"),
        state: Some("Closed"),
      ),
    ])

  let unparked = core.unpark_if_issue_changed(state, blockers_satisfied)
  assert !dict.has_key(unparked.parked, issue.id)
  assert core.should_dispatch(unparked, config(), blockers_satisfied)
}

fn assert_auto_park_blocks(
  state: domain.RuntimeState,
  changed: domain.Issue,
) -> Nil {
  let kept = core.unpark_if_issue_changed(state, changed)
  assert dict.has_key(kept.parked, changed.id)
  assert !core.should_dispatch(kept, config(), changed)
}

pub fn auto_park_clears_on_core_issue_change_test() {
  let issue = rich_issue()
  let state =
    domain.RuntimeState(
      ..core.new_state(config()),
      claimed: dict.from_list([#(issue.id, issue.identifier)]),
      retry_attempts: dict.from_list([
        #(issue.id, domain.RetryEntry(issue.id, 1000, 1)),
      ]),
      issue_counters: dict.from_list([
        #(
          issue.id,
          domain.IssueCounter(failure_attempts: 1, worker_sessions: 1),
        ),
      ]),
      parked: dict.from_list([
        #(issue.id, auto_parked_entry(issue, "max_retry_attempts")),
      ]),
    )
  let changed =
    domain.Issue(
      ..issue,
      title: "New title",
      blocked_by: [],
      updated_at: Some(birl.from_unix(2)),
    )

  let unparked = core.unpark_if_issue_changed(state, changed)
  assert !dict.has_key(unparked.parked, issue.id)
  assert !dict.has_key(unparked.claimed, issue.id)
  assert !dict.has_key(unparked.retry_attempts, issue.id)
  assert !dict.has_key(unparked.issue_counters, issue.id)
  assert core.should_dispatch(unparked, config(), changed)
}

pub fn reconciliation_and_token_accounting_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let terminal = domain.Issue(..issue, state: "Done")
  let core.Transition(effects: effects, state: next) =
    core.reconcile_issue(state, config(), terminal)
  assert effects
    == [core.StopWorker("a", "terminal"), core.CleanupWorkspace("/tmp/ws")]
  assert !dict.has_key(next.running, "a")

  let totals =
    core.add_tokens(
      domain.TokenTotals(
        input: 1,
        output: 2,
        cache_read: 3,
        cache_write: 4,
        total: 10,
      ),
      domain.TokenTotals(
        input: 2,
        output: 3,
        cache_read: 4,
        cache_write: 5,
        total: 14,
      ),
    )
  assert totals.total == 24
}
