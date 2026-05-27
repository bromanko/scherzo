import birl
import gleam/dict
import gleam/option.{type Option, None, Some}
import scherzo/config/types as config_types
import scherzo/orchestrator/core
import scherzo/orchestrator/reason
import scherzo/orchestrator/state as orchestrator_state
import scherzo/session/tokens as session_tokens
import scherzo/task
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/kind as tracker_kind
import scherzo/tracker/state as issue_state
import scherzo/workflow_completion_policy
import scherzo/workflow_policy

fn config() -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    tracker: config_types.TrackerConfig(
      kind: tracker_kind.LinearTracker,
      endpoint: "endpoint",
      api_key: Some("key"),
      project_slug: Some("PROJ"),
      active_states: issue_state.list_from_strings(["Todo", "In Progress"]),
      dispatch_states: issue_state.list_from_strings(["Todo"]),
      terminal_states: issue_state.list_from_strings([
        "Done",
        "Closed",
        "Canceled",
        "Cancelled",
        "Duplicate",
      ]),
    ),
    polling: config_types.PollingConfig(interval_ms: 30_000),
    workspace: config_types.WorkspaceConfig(root: "test/tmp/workspaces"),
    hooks: config_types.HooksConfig(
      after_create: Some("true"),
      before_run: None,
      after_run: None,
      before_remove: None,
      timeout_ms: 1000,
    ),
    agent: config_types.AgentConfig(
      max_concurrent_agents: 2,
      max_turns: 20,
      max_retry_backoff_ms: 40_000,
      max_retry_attempts: 3,
      max_sessions_per_issue: 2,
      context_recovery_max_attempts: 1,
      context_recovery_prompt_char_limit: 40_000,
      max_concurrent_agents_by_state: dict.from_list([
        #(issue_state.key_from_string("todo"), 1),
      ]),
    ),
    pi: config_types.PiConfig(
      command: "fake",
      turn_timeout_ms: 1000,
      read_timeout_ms: 1000,
      stall_timeout_ms: 1000,
      auto_retry: True,
      ui_request_policy: config_types.Cancel,
      ui_request_timeout_ms: 300_000,
      compatibility_probe: True,
      rate_limit_payload: None,
      argv_command: None,
      session_persistence: config_types.PiSessionPersistenceConfig(
        enabled: False,
        recovery_prompt: "",
      ),
    ),
    handoff: config_types.HandoffConfig(
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
    ),
    linear_contract: config_types.LinearContractConfig(
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
    ),
    linear_commands: config_types.LinearCommandConfig(
      enabled: False,
      prefix: "/scherzo",
      authorized_user_ids: [],
      poll_limit_per_issue: 25,
      max_comments_per_tick: 50,
      acknowledge_success: True,
      acknowledge_rejection: True,
    ),
    ui_server: config_types.UiServerConfig(
      enabled: False,
      endpoint: None,
      enrollment_token_env: None,
      enrollment_token: None,
    ),
  )
}

fn enforcing_config() -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    ..config(),
    linear_contract: config_types.LinearContractConfig(
      ..config().linear_contract,
      workflow_labels: ["bugfix", "research"],
      enforce_issue_workflow_labels: True,
    ),
  )
}

fn config_with_failure_state(
  state_name: String,
) -> config_types.EffectiveConfig {
  config_types.EffectiveConfig(
    ..config(),
    handoff: config_types.HandoffConfig(
      ..config().handoff,
      completion_states: Some(workflow_completion_policy.CompletionStatePolicy(
        default_completion_state: workflow_completion_policy.StateByName(
          "In Review",
        ),
        no_review_completion_state: Some(workflow_completion_policy.StateByName(
          "Done",
        )),
        failure_state: workflow_completion_policy.StateByName(state_name),
        partial_success_state: workflow_completion_policy.StateByName(
          state_name,
        ),
        cancellation_state: None,
        workflows: dict.new(),
      )),
    ),
  )
}

fn issue(
  id: String,
  identifier: String,
  state: String,
  priority: Option(Int),
) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: priority,
    state: issue_state.from_string_unchecked(state),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn task_item(
  backend_kind: String,
  remote_id: String,
  identifier: String,
  state: String,
) -> task.Task {
  task.Task(
    ref: task.TaskRef(
      backend_kind: backend_kind,
      remote_id: remote_id,
      key: Some(identifier),
      url: None,
    ),
    title: "Title " <> identifier,
    description: None,
    priority: None,
    state: task.TaskState(id: None, name: state, category: task.Unknown),
    branch_hint: None,
    labels: [],
    blockers: [],
    blockers_complete: True,
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn rich_issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    ..issue("a", "ABC-1", "Todo", Some(1)),
    description: Some("Description"),
    branch_name: Some("abc-1-title"),
    url: Some("https://linear.app/example/ABC-1"),
    labels: ["bug", "backend"],
    blocked_by: [
      tracker_issue.BlockerRef(
        id: Some("block-1"),
        identifier: Some("ABC-0"),
        state: Some(issue_state.from_string_unchecked("Todo")),
      ),
      tracker_issue.BlockerRef(
        id: Some("block-2"),
        identifier: Some("ABC-00"),
        state: Some(issue_state.from_string_unchecked("In Progress")),
      ),
    ],
    created_at: Some(birl.from_unix(1)),
    updated_at: Some(birl.from_unix(2)),
  )
}

fn auto_parked_entry(
  issue: tracker_issue.Issue,
  reason: reason.ParkReason,
) -> orchestrator_state.ParkedEntry {
  orchestrator_state.ParkedEntry(
    task_ref: task.from_legacy_issue(issue).ref,
    issue_id: issue.id,
    identifier: issue.identifier,
    reason: reason,
    release_policy: orchestrator_state.AutoUnparkOnIssueChange(
      core.issue_fingerprint(issue),
    ),
    parked_at_ms: 0,
  )
}

fn explicit_parked_entry(
  issue: tracker_issue.Issue,
  reason: reason.ParkReason,
) -> orchestrator_state.ParkedEntry {
  orchestrator_state.ParkedEntry(
    task_ref: task.from_legacy_issue(issue).ref,
    issue_id: issue.id,
    identifier: issue.identifier,
    reason: reason,
    release_policy: orchestrator_state.ExplicitUnparkOnly,
    parked_at_ms: 0,
  )
}

fn state_with_parked(
  state: orchestrator_state.RuntimeState,
  issue: tracker_issue.Issue,
  parked: orchestrator_state.ParkedEntry,
) -> orchestrator_state.RuntimeState {
  orchestrator_state.RuntimeState(
    ..state,
    parked: dict.insert(
      state.parked,
      orchestrator_state.issue_identity(issue),
      parked,
    ),
  )
}

pub fn running_state_distinguishes_duplicate_remote_ids_by_backend_test() {
  let linear_issue = issue("shared", "ABC-1", "Todo", Some(1))
  let memory_task = task_item("test-memory", "shared", "MEM-1", "Todo")
  let task.Task(ref: memory_ref, ..) = memory_task

  let state =
    core.new_state(config())
    |> core.apply_worker_start(linear_issue, "/tmp/linear")
    |> core.apply_task_start(memory_task, "/tmp/memory")

  let linear_identity = orchestrator_state.issue_identity(linear_issue)
  let memory_identity = orchestrator_state.task_ref_identity(memory_ref)
  assert linear_identity != memory_identity
  assert dict.size(state.running) == 2
  assert dict.has_key(state.running, linear_identity)
  assert dict.has_key(state.running, memory_identity)
}

pub fn worker_success_uses_task_ref_with_duplicate_remote_ids_test() {
  let linear_issue = issue("shared", "ABC-1", "Todo", Some(1))
  let memory_task = task_item("test-memory", "shared", "MEM-1", "Done")
  let task.Task(ref: memory_ref, ..) = memory_task
  let memory_issue = task.to_runtime_issue(memory_task)
  let state =
    core.new_state(config())
    |> core.apply_worker_start(linear_issue, "/tmp/linear")
    |> core.apply_task_start(memory_task, "/tmp/memory")

  let core.Transition(state: next, effects:) =
    core.apply_task_workflow_success(
      state,
      config(),
      memory_ref,
      memory_issue.id,
      memory_issue,
      session_tokens.zero_token_totals(),
      100,
      core.AlreadyCleaned,
    )

  let linear_identity = orchestrator_state.issue_identity(linear_issue)
  let memory_identity = orchestrator_state.task_ref_identity(memory_ref)
  assert dict.has_key(next.running, linear_identity)
  assert !dict.has_key(next.running, memory_identity)
  assert !dict.has_key(next.completed, linear_identity)
  assert dict.get(next.completed, memory_identity) == Ok(memory_issue)
  assert effects == [core.ReleaseClaim(memory_issue.id)]
}

pub fn worker_failure_retry_uses_task_ref_with_duplicate_remote_ids_test() {
  let linear_issue = issue("shared", "ABC-1", "Todo", Some(1))
  let memory_task = task_item("test-memory", "shared", "MEM-1", "Todo")
  let task.Task(ref: memory_ref, ..) = memory_task
  let memory_issue = task.to_runtime_issue(memory_task)
  let state =
    core.new_state(config())
    |> core.apply_worker_start(linear_issue, "/tmp/linear")
    |> core.apply_task_start(memory_task, "/tmp/memory")

  let core.Transition(state: next, effects:) =
    core.apply_task_worker_failure(
      state,
      config(),
      memory_ref,
      memory_issue.id,
      memory_issue,
      100,
    )

  let linear_identity = orchestrator_state.issue_identity(linear_issue)
  let memory_identity = orchestrator_state.task_ref_identity(memory_ref)
  assert dict.has_key(next.running, linear_identity)
  assert !dict.has_key(next.running, memory_identity)
  assert !dict.has_key(next.retry_attempts, linear_identity)
  assert dict.has_key(next.retry_attempts, memory_identity)
  assert effects
    == [
      core.CancelRetry(memory_issue.id, 1, "reschedule_retry"),
      core.ScheduleRetry(memory_issue.id, 10_000, 1, reason.RetryAfterFailure),
    ]
}

pub fn issue_fingerprint_ignores_timestamps_url_labels_and_state_test() {
  let base = rich_issue()
  let metadata_changed =
    tracker_issue.Issue(
      ..base,
      state: issue_state.from_string_unchecked("In Progress"),
      url: Some("https://linear.app/example/ABC-1?metadata=changed"),
      labels: ["frontend", "needs-review"],
      created_at: Some(birl.from_unix(100)),
      updated_at: Some(birl.from_unix(101)),
    )

  assert core.issue_fingerprint(base)
    == core.issue_fingerprint(metadata_changed)
}

pub fn legacy_stateful_issue_fingerprint_matches_content_fingerprint_test() {
  let legacy_todo_fingerprint =
    "1:a|5:ABC-1|11:Title ABC-1|none|some:1:1|4:Todo|none|4:true|"
  let current =
    tracker_issue.Issue(
      ..issue("a", "ABC-1", "In Progress", Some(1)),
      updated_at: Some(birl.from_unix(101)),
    )

  assert tracker_issue.fingerprint_matches(legacy_todo_fingerprint, current)
}

pub fn issue_fingerprint_changes_for_blockers_test() {
  let first =
    tracker_issue.BlockerRef(
      id: Some("block-1"),
      identifier: Some("ABC-0"),
      state: Some(issue_state.from_string_unchecked("Todo")),
    )
  let second =
    tracker_issue.BlockerRef(
      id: Some("block-2"),
      identifier: Some("ABC-00"),
      state: Some(issue_state.from_string_unchecked("In Progress")),
    )
  let base = tracker_issue.Issue(..rich_issue(), blocked_by: [first, second])
  let reordered = tracker_issue.Issue(..base, blocked_by: [second, first])
  let blocker_changed =
    tracker_issue.Issue(..base, blocked_by: [
      tracker_issue.BlockerRef(
        ..first,
        state: Some(issue_state.from_string_unchecked("Done")),
      ),
      second,
    ])

  assert core.issue_fingerprint(base) == core.issue_fingerprint(reordered)
  assert core.issue_fingerprint(base) != core.issue_fingerprint(blocker_changed)
  assert core.issue_fingerprint(base)
    != core.issue_fingerprint(
      tracker_issue.Issue(..base, blocked_by_complete: False),
    )
}

pub fn issue_fingerprint_changes_for_core_fields_test() {
  let base = rich_issue()
  let base_fingerprint = core.issue_fingerprint(base)

  assert core.issue_fingerprint(tracker_issue.Issue(..base, id: "different-id"))
    != base_fingerprint
  assert core.issue_fingerprint(
      tracker_issue.Issue(..base, identifier: "ABC-2"),
    )
    != base_fingerprint
  assert core.issue_fingerprint(
      tracker_issue.Issue(..base, title: "Different title"),
    )
    != base_fingerprint
  assert core.issue_fingerprint(
      tracker_issue.Issue(..base, description: Some("New")),
    )
    != base_fingerprint
  assert core.issue_fingerprint(tracker_issue.Issue(..base, priority: Some(2)))
    != base_fingerprint
  assert core.issue_fingerprint(
      tracker_issue.Issue(..base, branch_name: Some("new")),
    )
    != base_fingerprint
}

pub fn candidate_sorting_and_eligibility_test() {
  let a = issue("a", "ABC-2", "Todo", None)
  let b = issue("b", "ABC-1", "Todo", Some(1))
  assert core.sort_candidates([a, b]) == [b, a]

  let state = core.new_state(config())
  assert core.should_dispatch(state, config(), b)
  assert !core.should_dispatch(
    state,
    config(),
    tracker_issue.Issue(..b, id: ""),
  )
  assert !core.should_dispatch(
    state,
    config(),
    tracker_issue.Issue(
      ..b,
      state: issue_state.from_string_unchecked("In Progress"),
    ),
  )
  assert !core.should_dispatch(
    state,
    config(),
    tracker_issue.Issue(..b, state: issue_state.from_string_unchecked("Done")),
  )
}

pub fn workflow_policy_rejects_invalid_dispatch_test() {
  let base = issue("a", "ABC-1", "Todo", Some(1))
  let state = core.new_state(enforcing_config())
  assert core.should_dispatch(
    state,
    enforcing_config(),
    tracker_issue.Issue(..base, labels: ["workflow:bugfix"]),
  )
  assert !core.should_dispatch(state, enforcing_config(), base)
  assert !core.should_dispatch(
    state,
    enforcing_config(),
    tracker_issue.Issue(..base, labels: ["workflow:bugfix", "workflow:research"]),
  )
  assert !core.should_dispatch(
    state,
    enforcing_config(),
    tracker_issue.Issue(..base, labels: ["workflow:unknown"]),
  )
  assert core.should_dispatch(state, config(), base)
}

pub fn dispatch_preconditions_skip_parked_before_workflow_reporting_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let parked =
    state_with_parked(
      core.new_state(enforcing_config()),
      issue,
      explicit_parked_entry(issue, reason.ParkOperator("operator_hold")),
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
  assert core.retry_candidate_precondition_failure(
      retry_state,
      enforcing_config(),
      "a",
      issue,
    )
    == None
  let core.Transition(state: stopped, effects:) =
    core.stop_retry_for_policy_invalid(retry_state, "a")
  let identity = orchestrator_state.issue_identity(issue)
  assert !dict.has_key(stopped.retry_attempts, identity)
  assert !dict.has_key(stopped.claimed, identity)
  assert effects
    == [core.CancelRetry("a", 1, "policy_invalid"), core.ReleaseClaim("a")]
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
    tracker_issue.Issue(..issue, labels: ["workflow:unknown"], updated_at: None)
  assert !core.already_attempted_invalid_workflow(
    pending,
    changed,
    workflow_policy.UnknownWorkflowLabel("workflow:unknown"),
    enforcing_config().linear_contract,
  )
  let unknown_issue =
    tracker_issue.Issue(
      ..issue,
      labels: ["workflow:surprise"],
      updated_at: None,
    )
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
    tracker_issue.Issue(..unknown_issue, labels: ["workflow:other"]),
    workflow_policy.UnknownWorkflowLabel("workflow:other"),
    enforcing_config().linear_contract,
  )

  let changed_policy =
    config_types.LinearContractConfig(
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
  let assert Ok(report) =
    dict.get(
      updated.invalid_workflow_reports,
      orchestrator_state.issue_identity(issue),
    )
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
  assert !dict.has_key(
    cleared.invalid_workflow_reports,
    orchestrator_state.issue_identity(issue),
  )
}

pub fn running_claimed_parked_and_slots_reject_dispatch_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state = core.new_state(config())
  let running = core.apply_worker_start(state, issue, "/tmp/ws")
  assert !core.should_dispatch(running, config(), issue)

  let claimed =
    orchestrator_state.RuntimeState(
      ..state,
      claimed: dict.from_list([
        #(orchestrator_state.issue_identity(issue), "ABC-1"),
      ]),
    )
  assert !core.should_dispatch(claimed, config(), issue)

  let parked =
    state_with_parked(
      state,
      issue,
      explicit_parked_entry(issue, reason.ParkOperator("operator_hold")),
    )
  assert !core.should_dispatch(parked, config(), issue)

  let paused =
    config_types.EffectiveConfig(
      ..config(),
      agent: config_types.AgentConfig(
        ..config().agent,
        max_concurrent_agents: 0,
      ),
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
    tracker_issue.BlockerRef(
      id: Some("block"),
      identifier: Some("B-1"),
      state: Some(issue_state.from_string_unchecked("Todo")),
    )
  assert !core.should_dispatch(
    core.new_state(config()),
    config(),
    tracker_issue.Issue(..second, blocked_by: [blocker]),
  )
  let done_blocker =
    tracker_issue.BlockerRef(
      ..blocker,
      state: Some(issue_state.from_string_unchecked("Done")),
    )
  assert core.should_dispatch(
    core.new_state(config()),
    config(),
    tracker_issue.Issue(..second, blocked_by: [done_blocker]),
  )
}

pub fn blocker_decision_applies_to_all_active_states_test() {
  let base = issue("a", "ABC-1", "Todo", Some(1))
  let todo_blocker =
    tracker_issue.BlockerRef(
      id: Some("block-todo"),
      identifier: Some("B-1"),
      state: Some(issue_state.from_string_unchecked("Todo")),
    )
  let in_progress_blocker =
    tracker_issue.BlockerRef(
      id: Some("block-progress"),
      identifier: Some("B-2"),
      state: Some(issue_state.from_string_unchecked("In Progress")),
    )
  let backlog_blocker =
    tracker_issue.BlockerRef(
      id: Some("block-backlog"),
      identifier: Some("B-3"),
      state: Some(issue_state.from_string_unchecked("Backlog")),
    )
  let done_blocker =
    tracker_issue.BlockerRef(
      id: Some("block-done"),
      identifier: Some("B-4"),
      state: Some(issue_state.from_string_unchecked("Done")),
    )
  let canceled_blocker =
    tracker_issue.BlockerRef(
      id: Some("block-canceled"),
      identifier: Some("B-5"),
      state: Some(issue_state.from_string_unchecked("Canceled")),
    )
  let unknown_blocker =
    tracker_issue.BlockerRef(
      id: Some("block-unknown"),
      identifier: Some("B-6"),
      state: None,
    )

  assert core.blocker_decision(config(), base) == core.BlockersSatisfied
  assert !core.should_dispatch(
    core.new_state(config()),
    config(),
    tracker_issue.Issue(..base, blocked_by: [todo_blocker]),
  )
  assert !core.should_dispatch(
    core.new_state(config()),
    config(),
    tracker_issue.Issue(..base, blocked_by: [in_progress_blocker]),
  )
  assert !core.should_dispatch(
    core.new_state(config()),
    config(),
    tracker_issue.Issue(..base, blocked_by: [backlog_blocker]),
  )
  assert core.should_dispatch(
    core.new_state(config()),
    config(),
    tracker_issue.Issue(..base, blocked_by: [done_blocker]),
  )
  assert core.should_dispatch(
    core.new_state(config()),
    config(),
    tracker_issue.Issue(..base, blocked_by: [canceled_blocker]),
  )
  assert !core.should_dispatch(
    core.new_state(config()),
    config(),
    tracker_issue.Issue(..base, blocked_by: [unknown_blocker]),
  )
  assert !core.should_dispatch(
    core.new_state(config()),
    config(),
    tracker_issue.Issue(..base, blocked_by_complete: False),
  )

  let in_progress_issue =
    tracker_issue.Issue(
      ..base,
      state: issue_state.from_string_unchecked("In Progress"),
      blocked_by: [todo_blocker],
    )
  assert !core.should_dispatch(
    core.new_state(config()),
    config(),
    in_progress_issue,
  )
}

pub fn blocked_dependency_report_cache_test() {
  let issue =
    tracker_issue.Issue(..issue("a", "ABC-1", "Todo", Some(1)), blocked_by: [
      tracker_issue.BlockerRef(
        id: Some("block"),
        identifier: Some("B-1"),
        state: Some(issue_state.from_string_unchecked("Todo")),
      ),
    ])
  let decision = core.blocker_decision(config(), issue)
  let state = core.new_state(config())
  assert !core.already_reported_blocked_dependency(
    state,
    config(),
    issue,
    "candidate",
    decision,
  )
  let marked =
    core.mark_blocked_dependency_reported(
      state,
      config(),
      issue,
      "candidate",
      decision,
      100,
    )
  assert core.already_reported_blocked_dependency(
    marked,
    config(),
    issue,
    "candidate",
    decision,
  )
  assert !core.already_reported_blocked_dependency(
    marked,
    config(),
    tracker_issue.Issue(..issue, blocked_by_complete: False),
    "candidate",
    core.blocker_decision(
      config(),
      tracker_issue.Issue(..issue, blocked_by_complete: False),
    ),
  )
  assert !core.already_reported_blocked_dependency(
    marked,
    config(),
    issue,
    "retry",
    decision,
  )
  let cleared =
    core.clear_blocked_dependency_report(marked, issue.id, "candidate")
  assert !core.already_reported_blocked_dependency(
    cleared,
    config(),
    issue,
    "candidate",
    decision,
  )
}

pub fn worker_success_terminal_cleans_and_releases_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let final =
    tracker_issue.Issue(
      ..issue,
      state: issue_state.from_string_unchecked("Done"),
    )
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: next, effects:) =
    core.apply_worker_success_with_workspace_path(
      state,
      config(),
      "a",
      final,
      "/tmp/ws",
      session_tokens.zero_token_totals(),
      100,
    )
  let identity = orchestrator_state.issue_identity(issue)
  assert dict.has_key(next.running, identity) == False
  assert dict.has_key(next.claimed, identity) == False
  assert effects == [core.CleanupWorkspace("/tmp/ws"), core.ReleaseClaim("a")]
}

pub fn workflow_success_active_state_completes_without_retry_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let tokens =
    session_tokens.TokenTotals(
      input: 1,
      output: 2,
      cache_read: 3,
      cache_write: 4,
      total: 10,
    )
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: next, effects:) =
    core.apply_workflow_success(
      state,
      config(),
      "a",
      issue,
      tokens,
      100,
      core.AlreadyCleaned,
    )

  let identity = orchestrator_state.issue_identity(issue)
  assert !dict.has_key(next.running, identity)
  assert !dict.has_key(next.claimed, identity)
  assert dict.has_key(next.completed, identity)
  assert next.aggregate_pi_totals.total == 10
  assert next.aggregate_pi_totals.input == 1
  assert !dict.has_key(next.retry_attempts, identity)
  assert effects == [core.ReleaseClaim("a")]
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
      session_tokens.zero_token_totals(),
      100,
    )
  assert effects
    == [
      core.CancelRetry("a", 1, "reschedule_retry"),
      core.ScheduleRetry("a", 1000, 1, reason.RetryAfterContinuation),
    ]

  let state2 = core.apply_worker_start(next, issue, "/tmp/ws")
  let core.Transition(state: parked, effects: park_effects) =
    core.apply_worker_success_with_workspace_path(
      state2,
      config(),
      "a",
      issue,
      "/tmp/ws",
      session_tokens.zero_token_totals(),
      200,
    )
  let identity = orchestrator_state.issue_identity(issue)
  assert dict.has_key(parked.parked, identity)
  let assert Ok(parked_entry) = dict.get(parked.parked, identity)
  assert parked_entry.release_policy
    == orchestrator_state.AutoUnparkOnIssueChange(core.issue_fingerprint(issue))
  assert park_effects
    == [
      core.ParkIssue("a", reason.ParkMaxSessionsPerIssue),
      core.ReleaseClaim("a"),
    ]
}

pub fn worker_success_in_progress_remains_lifecycle_active_test() {
  let initial = issue("a", "ABC-1", "Todo", Some(1))
  let final =
    tracker_issue.Issue(
      ..initial,
      state: issue_state.from_string_unchecked("In Progress"),
    )
  let state =
    core.apply_worker_start(core.new_state(config()), initial, "/tmp/ws")
  let core.Transition(state: next, effects:) =
    core.apply_worker_success_with_workspace_path(
      state,
      config(),
      "a",
      final,
      "/tmp/ws",
      session_tokens.zero_token_totals(),
      100,
    )

  assert dict.has_key(
    next.completed,
    orchestrator_state.issue_identity(initial),
  )
  assert effects
    == [
      core.CancelRetry("a", 1, "reschedule_retry"),
      core.ScheduleRetry("a", 1000, 1, reason.RetryAfterContinuation),
    ]
}

pub fn worker_failure_backoff_and_retry_cap_test() {
  assert core.backoff_delay(1, 40_000) == 10_000
  assert core.backoff_delay(2, 40_000) == 20_000
  assert core.backoff_delay(3, 40_000) == 40_000
  assert core.backoff_delay(1000, 40_000) == 40_000

  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: one, effects: effects1) =
    core.apply_worker_failure(state, config(), "a", issue, 100)
  assert effects1
    == [
      core.CancelRetry("a", 1, "reschedule_retry"),
      core.ScheduleRetry("a", 10_000, 1, reason.RetryAfterFailure),
    ]
  let state = core.apply_worker_start(one, issue, "/tmp/ws")
  let core.Transition(state: two, effects: _) =
    core.apply_worker_failure(state, config(), "a", issue, 200)
  let latest = tracker_issue.Issue(..issue, title: "Latest failure title")
  let state = core.apply_worker_start(two, issue, "/tmp/ws")
  let core.Transition(state: parked, effects: effects3) =
    core.apply_worker_failure(state, config(), "a", latest, 300)
  let identity = orchestrator_state.issue_identity(issue)
  assert dict.has_key(parked.parked, identity)
  let assert Ok(parked_entry) = dict.get(parked.parked, identity)
  assert parked_entry.release_policy
    == orchestrator_state.AutoUnparkOnIssueChange(core.issue_fingerprint(latest))
  assert parked_entry.release_policy
    != orchestrator_state.AutoUnparkOnIssueChange(core.issue_fingerprint(issue))
  assert effects3
    == [
      core.ParkIssue("a", reason.ParkMaxRetryAttempts),
      core.ReleaseClaim("a"),
    ]
}

pub fn worker_failure_uses_dispatched_issue_id_for_lifecycle_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let mismatched_final_issue =
    tracker_issue.Issue(
      ..issue,
      id: "different-id",
      identifier: "ABC-999",
      title: "Different issue",
    )
  let retry_cap_config =
    config_types.EffectiveConfig(
      ..config(),
      agent: config_types.AgentConfig(..config().agent, max_retry_attempts: 1),
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

  let identity = orchestrator_state.issue_identity(issue)
  assert !dict.has_key(parked.running, identity)
  assert !dict.has_key(
    parked.running,
    orchestrator_state.linear_issue_id_identity("different-id"),
  )
  assert dict.has_key(parked.parked, identity)
  assert !dict.has_key(
    parked.parked,
    orchestrator_state.linear_issue_id_identity("different-id"),
  )
  assert effects
    == [
      core.ParkIssue("a", reason.ParkMaxRetryAttempts),
      core.ReleaseClaim("a"),
    ]
}

pub fn retry_candidate_can_dispatch_self_claimed_issue_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: retry_state, effects: _) =
    core.apply_worker_failure(state, config(), "a", issue, 100)

  let updated = tracker_issue.Issue(..issue, title: "Updated title")
  let core.Transition(state: next, effects: effects) =
    core.handle_retry_candidate(retry_state, config(), "a", Ok(Some(updated)))

  assert effects
    == [
      core.CancelRetry("a", 1, "retry_dispatch"),
      core.Dispatch(updated),
    ]
  let identity = orchestrator_state.issue_identity(issue)
  assert !dict.has_key(next.retry_attempts, identity)
  assert dict.has_key(next.claimed, identity)
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
      session_tokens.zero_token_totals(),
      100,
    )

  let core.Transition(state: next, effects: effects) =
    core.handle_retry_candidate(retry_state, config(), "a", Ok(Some(issue)))

  assert effects
    == [
      core.CancelRetry("a", 1, "retry_dispatch"),
      core.Dispatch(issue),
    ]
  let identity = orchestrator_state.issue_identity(issue)
  assert !dict.has_key(next.retry_attempts, identity)
  assert dict.has_key(next.claimed, identity)
}

pub fn retry_candidate_terminal_issue_clears_retry_without_no_slots_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: retry_state, effects: _) =
    core.apply_worker_failure(state, config(), "a", issue, 100)
  let done =
    tracker_issue.Issue(
      ..issue,
      state: issue_state.from_string_unchecked("Done"),
    )

  let core.Transition(state: next, effects: effects) =
    core.handle_retry_candidate(retry_state, config(), "a", Ok(Some(done)))

  assert effects
    == [
      core.CancelRetry("a", 1, "retry_terminal_state:Done"),
      core.ReleaseClaim("a"),
    ]
  let identity = orchestrator_state.issue_identity(issue)
  assert !dict.has_key(next.retry_attempts, identity)
  assert !dict.has_key(next.claimed, identity)
}

pub fn retry_candidate_non_dispatch_failure_state_can_dispatch_test() {
  let config = config_with_failure_state("Triage")
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state = core.apply_worker_start(core.new_state(config), issue, "/tmp/ws")
  let core.Transition(state: retry_state, effects: _) =
    core.apply_worker_failure(state, config, "a", issue, 100)
  let triage =
    tracker_issue.Issue(
      ..issue,
      state: issue_state.from_string_unchecked("Triage"),
    )

  let core.Transition(state: next, effects: effects) =
    core.handle_retry_candidate(retry_state, config, "a", Ok(Some(triage)))

  assert effects
    == [
      core.CancelRetry("a", 1, "retry_dispatch"),
      core.Dispatch(triage),
    ]
  let identity = orchestrator_state.issue_identity(issue)
  assert !dict.has_key(next.retry_attempts, identity)
  assert dict.has_key(next.claimed, identity)
}

pub fn retry_candidate_non_retryable_state_clears_retry_test() {
  let config = config_with_failure_state("Triage")
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state = core.apply_worker_start(core.new_state(config), issue, "/tmp/ws")
  let core.Transition(state: retry_state, effects: _) =
    core.apply_worker_failure(state, config, "a", issue, 100)
  let backlog =
    tracker_issue.Issue(
      ..issue,
      state: issue_state.from_string_unchecked("Backlog"),
    )

  let core.Transition(state: next, effects: effects) =
    core.handle_retry_candidate(retry_state, config, "a", Ok(Some(backlog)))

  assert effects
    == [
      core.CancelRetry("a", 1, "retry_non_retryable_state:Backlog"),
      core.ReleaseClaim("a"),
    ]
  let identity = orchestrator_state.issue_identity(issue)
  assert !dict.has_key(next.retry_attempts, identity)
  assert !dict.has_key(next.claimed, identity)
}

pub fn retry_candidate_without_slot_capacity_retries_no_slots_test() {
  let retry_issue = issue("a", "ABC-1", "Todo", Some(1))
  let running_issue = issue("b", "ABC-2", "In Progress", Some(2))
  let one_slot_config =
    config_types.EffectiveConfig(
      ..config(),
      agent: config_types.AgentConfig(
        ..config().agent,
        max_concurrent_agents: 1,
      ),
    )
  let state =
    core.apply_worker_start(
      core.new_state(one_slot_config),
      retry_issue,
      "/tmp/a",
    )
  let core.Transition(state: retry_state, effects: _) =
    core.apply_worker_failure(state, one_slot_config, "a", retry_issue, 100)
  let full_state = core.apply_worker_start(retry_state, running_issue, "/tmp/b")

  let core.Transition(state: next, effects: effects) =
    core.handle_retry_candidate(
      full_state,
      one_slot_config,
      "a",
      Ok(Some(retry_issue)),
    )

  assert effects
    == [
      core.CancelRetry("a", 2, "reschedule_retry"),
      core.ScheduleRetry("a", 20_000, 2, reason.RetryNoSlots),
    ]
  let identity = orchestrator_state.issue_identity(retry_issue)
  assert dict.has_key(next.retry_attempts, identity)
  assert dict.has_key(next.claimed, identity)
}

pub fn retry_timer_handling_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let core.Transition(state: retry_state, effects: _) =
    core.apply_worker_failure(state, config(), "a", issue, 100)
  let core.Transition(effects: poll_failed, state: kept) =
    core.handle_retry_candidate(retry_state, config(), "a", Error("tracker"))
  assert dict.has_key(kept.claimed, orchestrator_state.issue_identity(issue))
  assert poll_failed
    == [
      core.CancelRetry("a", 2, "reschedule_retry"),
      core.ScheduleRetry("a", 20_000, 2, reason.RetryPollFailed),
    ]

  let core.Transition(effects: poll_failed_again, state: kept_again) =
    core.handle_retry_candidate(kept, config(), "a", Error("tracker"))
  assert poll_failed_again
    == [
      core.CancelRetry("a", 3, "reschedule_retry"),
      core.ScheduleRetry("a", 40_000, 3, reason.RetryPollFailed),
    ]

  let core.Transition(effects: absent, state: released) =
    core.handle_retry_candidate(kept_again, config(), "a", Ok(None))
  assert absent
    == [
      core.CancelRetry("a", 3, "retry_issue_missing"),
      core.ReleaseClaim("a"),
    ]
  assert !dict.has_key(
    released.claimed,
    orchestrator_state.issue_identity(issue),
  )
}

pub fn retry_candidate_without_slots_uses_backoff_test() {
  let retry_issue = issue("a", "ABC-1", "Todo", Some(1))
  let occupying_issue = issue("b", "ABC-2", "Todo", Some(2))
  let state =
    core.apply_worker_start(core.new_state(config()), retry_issue, "/tmp/a")
  let core.Transition(state: retry_state, effects: _) =
    core.apply_worker_failure(state, config(), "a", retry_issue, 100)
  let occupied = core.apply_worker_start(retry_state, occupying_issue, "/tmp/b")

  let core.Transition(state: _, effects: effects) =
    core.handle_retry_candidate(occupied, config(), "a", Ok(Some(retry_issue)))

  assert effects
    == [
      core.CancelRetry("a", 2, "reschedule_retry"),
      core.ScheduleRetry("a", 20_000, 2, reason.RetryNoSlots),
    ]
}

pub fn explicit_park_blocks_even_when_issue_changes_test() {
  let issue = rich_issue()
  let state =
    state_with_parked(
      core.new_state(config()),
      issue,
      explicit_parked_entry(issue, reason.ParkOperator("operator_abort")),
    )
  let changed =
    tracker_issue.Issue(
      ..issue,
      title: "Changed title",
      description: Some("Changed description"),
      updated_at: Some(birl.from_unix(999)),
    )

  let kept = core.unpark_if_issue_changed(state, changed)
  assert dict.has_key(kept.parked, orchestrator_state.issue_identity(issue))
  assert !core.should_dispatch(kept, config(), changed)
}

pub fn auto_park_ignores_comment_and_non_core_changes_test() {
  let issue = rich_issue()
  let state =
    state_with_parked(
      core.new_state(config()),
      issue,
      auto_parked_entry(issue, reason.ParkMaxRetryAttempts),
    )

  let timestamp_changed =
    tracker_issue.Issue(..issue, updated_at: Some(birl.from_unix(200)))
  assert_auto_park_blocks(state, timestamp_changed)

  let url_changed =
    tracker_issue.Issue(..issue, url: Some("https://example.invalid"))
  assert_auto_park_blocks(state, url_changed)

  let labels_changed = tracker_issue.Issue(..issue, labels: ["new", "metadata"])
  assert_auto_park_blocks(state, labels_changed)

  let handoff_state_changed =
    tracker_issue.Issue(
      ..issue,
      state: issue_state.from_string_unchecked("In Progress"),
    )
  assert_auto_park_blocks(state, handoff_state_changed)

  let terminal_state_changed =
    tracker_issue.Issue(
      ..issue,
      state: issue_state.from_string_unchecked("Done"),
    )
  assert_auto_park_blocks(state, terminal_state_changed)
}

pub fn auto_park_clears_on_blocker_change_test() {
  let issue = rich_issue()
  let state =
    state_with_parked(
      core.new_state(config()),
      issue,
      auto_parked_entry(issue, reason.ParkMaxRetryAttempts),
    )
  let blockers_satisfied =
    tracker_issue.Issue(..issue, blocked_by: [
      tracker_issue.BlockerRef(
        id: Some("block-1"),
        identifier: Some("ABC-0"),
        state: Some(issue_state.from_string_unchecked("Done")),
      ),
      tracker_issue.BlockerRef(
        id: Some("block-2"),
        identifier: Some("ABC-00"),
        state: Some(issue_state.from_string_unchecked("Closed")),
      ),
    ])

  let unparked = core.unpark_if_issue_changed(state, blockers_satisfied)
  assert !dict.has_key(
    unparked.parked,
    orchestrator_state.issue_identity(issue),
  )
  assert core.should_dispatch(unparked, config(), blockers_satisfied)
}

fn assert_auto_park_blocks(
  state: orchestrator_state.RuntimeState,
  changed: tracker_issue.Issue,
) -> Nil {
  let kept = core.unpark_if_issue_changed(state, changed)
  assert dict.has_key(kept.parked, orchestrator_state.issue_identity(changed))
  assert !core.should_dispatch(kept, config(), changed)
}

pub fn auto_park_clears_on_core_issue_change_test() {
  let issue = rich_issue()
  let state =
    orchestrator_state.RuntimeState(
      ..core.new_state(config()),
      claimed: dict.from_list([
        #(orchestrator_state.issue_identity(issue), issue.identifier),
      ]),
      retry_attempts: dict.from_list([
        #(
          orchestrator_state.issue_identity(issue),
          orchestrator_state.RetryEntry(
            task_ref: task.from_legacy_issue(issue).ref,
            issue_id: issue.id,
            delay_ms: 1000,
            timer_generation: 1,
          ),
        ),
      ]),
      issue_counters: dict.from_list([
        #(
          orchestrator_state.issue_identity(issue),
          orchestrator_state.IssueCounter(
            failure_attempts: 1,
            worker_sessions: 1,
          ),
        ),
      ]),
      parked: dict.from_list([
        #(
          orchestrator_state.issue_identity(issue),
          auto_parked_entry(issue, reason.ParkMaxRetryAttempts),
        ),
      ]),
    )
  let changed =
    tracker_issue.Issue(
      ..issue,
      title: "New title",
      blocked_by: [],
      blocked_by_complete: True,
      updated_at: Some(birl.from_unix(2)),
    )

  let unparked = core.unpark_if_issue_changed(state, changed)
  assert !dict.has_key(
    unparked.parked,
    orchestrator_state.issue_identity(issue),
  )
  assert !dict.has_key(
    unparked.claimed,
    orchestrator_state.issue_identity(issue),
  )
  assert !dict.has_key(
    unparked.retry_attempts,
    orchestrator_state.issue_identity(issue),
  )
  assert !dict.has_key(
    unparked.issue_counters,
    orchestrator_state.issue_identity(issue),
  )
  assert core.should_dispatch(unparked, config(), changed)
}

pub fn reconciliation_and_token_accounting_test() {
  let issue = issue("a", "ABC-1", "Todo", Some(1))
  let state =
    core.apply_worker_start(core.new_state(config()), issue, "/tmp/ws")
  let terminal =
    tracker_issue.Issue(
      ..issue,
      state: issue_state.from_string_unchecked("Done"),
    )
  let core.Transition(effects: effects, state: next) =
    core.reconcile_issue(state, config(), terminal)
  assert effects
    == [
      core.StopWorker("a", reason.StopTerminal),
      core.CleanupWorkspace("/tmp/ws"),
    ]
  assert !dict.has_key(next.running, orchestrator_state.issue_identity(issue))

  let totals =
    core.add_tokens(
      session_tokens.TokenTotals(
        input: 1,
        output: 2,
        cache_read: 3,
        cache_write: 4,
        total: 10,
      ),
      session_tokens.TokenTotals(
        input: 2,
        output: 3,
        cache_read: 4,
        cache_write: 5,
        total: 14,
      ),
    )
  assert totals.total == 24
}
