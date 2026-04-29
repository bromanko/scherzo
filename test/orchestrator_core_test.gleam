import birl
import gleam/dict
import gleam/option.{type Option, None, Some}
import scherzo/domain
import scherzo/orchestrator/core

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

pub fn running_claimed_parked_and_slots_reject_dispatch_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state = core.new_state(config())
  let running = core.apply_worker_start(state, issue, "/tmp/ws")
  assert !core.should_dispatch(running, config(), issue)

  let claimed =
    domain.RuntimeState(..state, claimed: dict.from_list([#("a", "ABC-1")]))
  assert !core.should_dispatch(claimed, config(), issue)

  let parked =
    domain.RuntimeState(
      ..state,
      parked: dict.from_list([
        #(
          "a",
          domain.ParkedEntry(
            issue_id: "a",
            identifier: "ABC-1",
            reason: "cap",
            observed_updated_at: issue.updated_at,
            parked_at_ms: 0,
          ),
        ),
      ]),
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
    core.apply_worker_failure(state, config(), "a", 100)
  assert effects1
    == [core.CancelRetry("a"), core.ScheduleRetry("a", 10_000, 1, "failure")]
  let state = core.apply_worker_start(one, issue, "/tmp/ws")
  let core.Transition(state: two, effects: _) =
    core.apply_worker_failure(state, config(), "a", 200)
  let state = core.apply_worker_start(two, issue, "/tmp/ws")
  let core.Transition(state: parked, effects: effects3) =
    core.apply_worker_failure(state, config(), "a", 300)
  assert dict.has_key(parked.parked, "a")
  assert effects3
    == [core.ParkIssue("a", "max_retry_attempts"), core.ReleaseClaim("a")]
}

pub fn retry_candidate_can_dispatch_self_claimed_issue_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: retry_state, effects: _) =
    core.apply_worker_failure(state, config(), "a", 100)

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

pub fn retry_timer_handling_and_unparking_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: retry_state, effects: _) =
    core.apply_worker_failure(state, config(), "a", 100)
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

  let parked =
    domain.RuntimeState(
      ..core.new_state(config()),
      claimed: dict.from_list([#("a", "ABC-1")]),
      parked: dict.from_list([
        #(
          "a",
          domain.ParkedEntry(
            issue_id: "a",
            identifier: "ABC-1",
            reason: "cap",
            observed_updated_at: Some(birl.from_unix(0)),
            parked_at_ms: 0,
          ),
        ),
      ]),
    )
  let updated = domain.Issue(..issue, updated_at: Some(birl.from_unix(1)))
  let unparked = core.unpark_if_updated(parked, updated)
  assert !dict.has_key(unparked.parked, "a")
  assert !dict.has_key(unparked.claimed, "a")
  assert core.should_dispatch(unparked, config(), updated)
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
