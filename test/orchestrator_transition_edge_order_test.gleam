import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import orchestrator_transition_test
import scherzo/agent/types as agent_types
import scherzo/control/command
import scherzo/orchestrator/effects/interpreter
import scherzo/orchestrator/task_lifecycle
import scherzo/orchestrator/transition
import scherzo/orchestrator/transition_types
import scherzo/result_artifact
import scherzo/runtime/identity
import scherzo/runtime/state as orchestrator_state
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger_batch
import scherzo/state/record
import scherzo/task
import scherzo/tracker/issue as tracker_issue

const generated_message_limit = 96

type GeneratedCase {
  GeneratedCase(
    initial: transition_types.State,
    messages: List(transition_types.Message),
    expectations: Expectations,
  )
}

type Expectations {
  Expectations(
    final_state_unchanged: Bool,
    observer_events_only: Bool,
    expected_start_count: Int,
    pending_claims_empty: Bool,
    expected_events: List(String),
  )
}

type RunResult {
  RunResult(
    state: transition_types.State,
    events: List(String),
    exhausted: Bool,
  )
}

pub fn generated_edge_order_sequences_are_safe_test() {
  generated_edge_cases()
  |> list.each(run_generated_case)
}

fn run_generated_case(test_case: GeneratedCase) -> Nil {
  let GeneratedCase(
    initial: initial,
    messages: messages,
    expectations: expectations,
  ) = test_case
  let result = run_sequence(initial, messages)

  assert result.exhausted == False
  assert_no_duplicate_worker_starts(result.events)
  assert list.length(start_events(result.events))
    == expectations.expected_start_count
  assert_events_present(result.events, expectations.expected_events)

  case expectations.final_state_unchanged {
    True -> {
      assert result.state == initial
    }
    False -> Nil
  }
  case expectations.observer_events_only {
    True -> assert_only_observer_events(result.events)
    False -> Nil
  }
  case expectations.pending_claims_empty {
    True -> {
      assert dict.size(result.state.pending_claims) == 0
    }
    False -> Nil
  }

  check_daemon_transition_invariants(result.state)
}

fn generated_edge_cases() -> List(GeneratedCase) {
  let issue = orchestrator_transition_test.fixture_issue()
  let other_issue =
    tracker_issue.Issue(..issue, id: "issue-2", identifier: "ABC-2")

  [
    GeneratedCase(
      initial: orchestrator_transition_test.fixture_state(),
      messages: stale_poll_messages(issue),
      expectations: Expectations(
        final_state_unchanged: True,
        observer_events_only: True,
        expected_start_count: 0,
        pending_claims_empty: True,
        expected_events: [],
      ),
    ),
    GeneratedCase(
      initial: state_with_pending_dispatch_validation(issue, 2),
      messages: [
        transition_types.DispatchValidationCompleted(
          issue.id,
          1,
          Ok(issue),
          orchestrator_transition_test.fixture_context(),
        ),
        transition_types.DispatchValidationCompleted(
          other_issue.id,
          2,
          Ok(other_issue),
          orchestrator_transition_test.fixture_context(),
        ),
      ],
      expectations: Expectations(
        final_state_unchanged: True,
        observer_events_only: True,
        expected_start_count: 0,
        pending_claims_empty: True,
        expected_events: ["log:dispatch_validation_stale"],
      ),
    ),
    GeneratedCase(
      initial: state_with_pending_dispatch_validation(issue, 1),
      messages: [
        transition_types.DispatchValidationCompleted(
          issue.id,
          1,
          Ok(issue),
          orchestrator_transition_test.fixture_context(),
        ),
        transition_types.DispatchValidationCompleted(
          issue.id,
          1,
          Ok(issue),
          orchestrator_transition_test.fixture_context(),
        ),
      ],
      expectations: Expectations(
        final_state_unchanged: False,
        observer_events_only: False,
        expected_start_count: 0,
        pending_claims_empty: False,
        expected_events: ["claim:issue-1", "log:dispatch_validation_stale"],
      ),
    ),
    GeneratedCase(
      initial: orchestrator_transition_test.state_with_pending_claim(issue),
      messages: [
        transition_types.HandoffClaimCompleted(
          orchestrator_state.linear_issue_id_identity("issue-2"),
          identity.issue_id_from_string(issue.id),
          identity.run_id_from_string("run-1"),
          transition_types.HandoffClaimSucceeded(claim_batch(issue)),
        ),
        transition_types.HandoffClaimCompleted(
          task_identity(issue),
          identity.issue_id_from_string(issue.id),
          identity.run_id_from_string("run-stale"),
          transition_types.HandoffClaimSucceeded(claim_batch(issue)),
        ),
      ],
      expectations: Expectations(
        final_state_unchanged: True,
        observer_events_only: True,
        expected_start_count: 0,
        pending_claims_empty: False,
        expected_events: ["log:handoff_claim_stale"],
      ),
    ),
    GeneratedCase(
      initial: orchestrator_transition_test.state_with_pending_claim(issue),
      messages: [valid_handoff_claim(issue), valid_handoff_claim(issue)],
      expectations: Expectations(
        final_state_unchanged: False,
        observer_events_only: False,
        expected_start_count: 1,
        pending_claims_empty: True,
        expected_events: [
          "append:claim:issue-1:run-1",
          "start:issue-1:run-1:session-1",
          "log:claim_ledger_continuation_stale",
        ],
      ),
    ),
    GeneratedCase(
      initial: state_with_retry(issue, 3),
      messages: [
        transition_types.RetryRefreshCompleted(
          issue.id,
          3,
          Ok([issue]),
          orchestrator_transition_test.fixture_context(),
        ),
        valid_retry_handoff_claim(issue),
      ],
      expectations: Expectations(
        final_state_unchanged: False,
        observer_events_only: False,
        expected_start_count: 1,
        pending_claims_empty: True,
        expected_events: [
          "retry:finish:issue-1",
          "claim:issue-1",
          "append:claim:issue-1:ABC-1-123-1",
          "retry:cancel:issue-1:3:retry_dispatch",
          "start:issue-1:ABC-1-123-1:ABC-1-123-1",
        ],
      ),
    ),
    GeneratedCase(
      initial: state_with_worker(issue, transition_types.WorkerStarting),
      messages: [
        transition_types.WorkerStartSucceeded(
          identity.issue_id_from_string(issue.id),
          identity.run_id_from_string("run-1"),
          identity.session_id_from_string("session-stale"),
        ),
        transition_types.WorkerStartFailed(
          identity.issue_id_from_string(issue.id),
          identity.run_id_from_string("run-stale"),
          identity.session_id_from_string("session-1"),
          "late_failure",
        ),
      ],
      expectations: Expectations(
        final_state_unchanged: True,
        observer_events_only: True,
        expected_start_count: 0,
        pending_claims_empty: True,
        expected_events: [
          "log:worker_start_stale",
          "log:worker_start_failed_stale",
        ],
      ),
    ),
    GeneratedCase(
      initial: state_with_worker(issue, transition_types.WorkerRunning),
      messages: [
        transition_types.WorkerFinished(
          identity.issue_id_from_string(issue.id),
          identity.run_id_from_string("run-stale"),
          Ok(worker_success(issue)),
          lifecycle_context(),
        ),
        transition_types.WorkerDown(
          transition_types.KnownWorkerDown(
            identity.issue_id_from_string(issue.id),
            identity.run_id_from_string("run-1"),
            identity.session_id_from_string("session-stale"),
          ),
          lifecycle_context(),
        ),
        transition_types.WorkerDown(
          transition_types.WorkerDownStale(identity.issue_id_from_string(
            issue.id,
          )),
          lifecycle_context(),
        ),
        transition_types.WorkerDown(
          transition_types.UnknownWorkerDown,
          lifecycle_context(),
        ),
      ],
      expectations: Expectations(
        final_state_unchanged: True,
        observer_events_only: True,
        expected_start_count: 0,
        pending_claims_empty: True,
        expected_events: [
          "log:worker_finished_stale",
          "log:worker_down_stale",
        ],
      ),
    ),
    GeneratedCase(
      initial: state_with_worker(issue, transition_types.WorkerRunning),
      messages: [
        transition_types.WorkerFinished(
          identity.issue_id_from_string(issue.id),
          identity.run_id_from_string("run-1"),
          Ok(worker_success(issue)),
          lifecycle_context(),
        ),
        transition_types.WorkerFinished(
          identity.issue_id_from_string(issue.id),
          identity.run_id_from_string("run-1"),
          Ok(worker_success(issue)),
          lifecycle_context(),
        ),
        transition_types.WorkerDown(
          transition_types.KnownWorkerDown(
            identity.issue_id_from_string(issue.id),
            identity.run_id_from_string("run-1"),
            identity.session_id_from_string("session-1"),
          ),
          lifecycle_context(),
        ),
      ],
      expectations: Expectations(
        final_state_unchanged: False,
        observer_events_only: False,
        expected_start_count: 0,
        pending_claims_empty: True,
        expected_events: [
          "remove:issue-1",
          "log:worker_exited",
          "release:issue-1",
          "log:worker_finished_stale",
        ],
      ),
    ),
    GeneratedCase(
      initial: state_with_retry(issue, 3),
      messages: [
        transition_types.RetryTick(
          issue.id,
          2,
          orchestrator_transition_test.fixture_context(),
        ),
        transition_types.RetryTick(
          "issue-missing",
          1,
          orchestrator_transition_test.fixture_context(),
        ),
        transition_types.RetryRefreshCompleted(
          issue.id,
          2,
          Ok([issue]),
          orchestrator_transition_test.fixture_context(),
        ),
        transition_types.RetryRefreshCompleted(
          "issue-missing",
          1,
          Ok([]),
          orchestrator_transition_test.fixture_context(),
        ),
      ],
      expectations: Expectations(
        final_state_unchanged: True,
        observer_events_only: True,
        expected_start_count: 0,
        pending_claims_empty: True,
        expected_events: ["log:retry_timer_stale", "retry:finish:issue-1"],
      ),
    ),
  ]
}

fn stale_poll_messages(
  issue: tracker_issue.Issue,
) -> List(transition_types.Message) {
  [
    transition_types.PollTick(
      0,
      transition_types.PollSnapshot(generation: 1, in_flight: None),
    ),
    transition_types.PollTick(
      1,
      transition_types.PollSnapshot(generation: 1, in_flight: Some(1)),
    ),
    transition_types.RunningRefreshCompleted(
      0,
      transition_types.PollSnapshot(generation: 1, in_flight: Some(1)),
      Ok([issue]),
      orchestrator_transition_test.fixture_context(),
    ),
    transition_types.CandidateFetchCompleted(
      2,
      transition_types.PollSnapshot(generation: 1, in_flight: Some(1)),
      Ok([issue]),
      orchestrator_transition_test.fixture_context(),
    ),
  ]
}

fn run_sequence(
  initial: transition_types.State,
  messages: List(transition_types.Message),
) -> RunResult {
  check_daemon_transition_invariants(initial)
  run_loop(initial, edge_shell(), messages, generated_message_limit)
}

fn run_loop(
  state: transition_types.State,
  shell: interpreter.ShellState(List(String)),
  messages: List(transition_types.Message),
  remaining: Int,
) -> RunResult {
  case messages {
    [] ->
      RunResult(state: state, events: interpreter.data(shell), exhausted: False)
    [message, ..rest] ->
      case remaining <= 0 {
        True ->
          RunResult(
            state: state,
            events: interpreter.data(shell),
            exhausted: True,
          )
        False -> {
          let transition_types.Outcome(state: next_state, effects: effects) =
            transition.handle(message, state)
          check_daemon_transition_invariants(next_state)
          let interpreter.ApplyResult(
            shell: next_shell,
            follow_up_messages: follow_up_messages,
          ) = interpreter.apply(shell, effects)
          run_loop(
            next_state,
            next_shell,
            list.append(rest, follow_up_messages),
            remaining - 1,
          )
        }
      }
  }
}

fn check_daemon_transition_invariants(state: transition_types.State) -> Nil {
  assert_slots_not_overcommitted(state)
  assert_worker_directory_consistent(state)
  assert_terminal_tasks_have_no_live_edges(state)
  assert_pending_edges_are_disjoint(state)
}

fn assert_slots_not_overcommitted(state: transition_types.State) -> Nil {
  let live_slots =
    dict.size(state.runtime.running)
    + dict.size(state.pending_claims)
    + dict.size(state.pending_dispatch_validations)

  case state.runtime.max_concurrent_agents {
    0 -> {
      assert live_slots == 0
    }
    max_concurrent_agents -> {
      assert live_slots <= max_concurrent_agents
    }
  }
}

fn assert_worker_directory_consistent(state: transition_types.State) -> Nil {
  state.workers.by_issue
  |> dict.to_list
  |> list.each(fn(worker_pair) {
    let #(worker_identity, worker) = worker_pair
    assert dict.get(state.workers.by_session, worker.session_id)
      == Ok(worker_identity)
    assert dict.get(state.workers.route_to_session, worker.command_route_id)
      == Ok(worker.session_id)
    assert dict.has_key(state.runtime.running, worker_identity)
  })

  state.workers.by_session
  |> dict.to_list
  |> list.each(fn(session_pair) {
    let #(session_id, worker_identity) = session_pair
    let assert Ok(worker) = dict.get(state.workers.by_issue, worker_identity)
    assert worker.session_id == session_id
  })

  state.workers.route_to_session
  |> dict.to_list
  |> list.each(fn(route_pair) {
    let #(route_id, session_id) = route_pair
    let assert Ok(worker_identity) =
      dict.get(state.workers.by_session, session_id)
    let assert Ok(worker) = dict.get(state.workers.by_issue, worker_identity)
    assert worker.command_route_id == route_id
  })

  let worker_sessions =
    state.workers.by_issue
    |> dict.values
    |> list.map(fn(worker) { worker.session_id })
  let worker_routes =
    state.workers.by_issue
    |> dict.values
    |> list.map(fn(worker) { worker.command_route_id })
  assert list.length(worker_sessions)
    == list.length(unique_strings(worker_sessions))
  assert list.length(worker_routes)
    == list.length(unique_strings(worker_routes))
}

fn assert_terminal_tasks_have_no_live_edges(
  state: transition_types.State,
) -> Nil {
  state.runtime.completed
  |> dict.to_list
  |> list.each(fn(completed_pair) {
    let #(task_identity, _) = completed_pair
    assert !dict.has_key(state.runtime.running, task_identity)
    assert !dict.has_key(state.workers.by_issue, task_identity)
    assert !dict.has_key(state.pending_claims, task_identity)
    assert !dict.has_key(state.pending_dispatch_validations, task_identity)
  })
}

fn assert_pending_edges_are_disjoint(state: transition_types.State) -> Nil {
  state.pending_claims
  |> dict.to_list
  |> list.each(fn(pending_pair) {
    let #(task_identity, pending) = pending_pair
    assert !dict.has_key(state.runtime.running, task_identity)
    assert !dict.has_key(state.pending_dispatch_validations, task_identity)
    assert pending.issue_id == pending.issue.id
  })

  state.pending_dispatch_validations
  |> dict.to_list
  |> list.each(fn(pending_pair) {
    let #(task_identity, pending) = pending_pair
    assert !dict.has_key(state.runtime.running, task_identity)
    assert !dict.has_key(state.pending_claims, task_identity)
    assert pending.issue.id == pending.task_ref.remote_id
  })
}

fn assert_no_duplicate_worker_starts(events: List(String)) -> Nil {
  let starts = start_events(events)
  assert list.length(starts) == list.length(unique_strings(starts))
}

fn assert_only_observer_events(events: List(String)) -> Nil {
  events
  |> list.each(fn(event) {
    assert string.starts_with(event, "log:")
      || string.starts_with(event, "retry:finish:")
  })
}

fn assert_events_present(events: List(String), expected: List(String)) -> Nil {
  expected
  |> list.each(fn(event) {
    assert list.contains(events, event)
  })
}

fn start_events(events: List(String)) -> List(String) {
  events
  |> list.filter(fn(event) { string.starts_with(event, "start:") })
}

fn unique_strings(values: List(String)) -> List(String) {
  list.fold(values, [], fn(unique, value) {
    case list.contains(unique, value) {
      True -> unique
      False -> [value, ..unique]
    }
  })
}

fn state_with_pending_dispatch_validation(
  issue: tracker_issue.Issue,
  generation: Int,
) -> transition_types.State {
  let task_ref = task.from_legacy_issue(issue).ref
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    pending_dispatch_validations: dict.from_list([
      #(
        task_identity(issue),
        transition_types.PendingDispatchValidation(
          task_ref: task_ref,
          issue: issue,
          remaining_candidates: [],
          generation: generation,
          requested_at_ms: 123,
        ),
      ),
    ]),
    lifecycle: {
      let assert Ok(directory) =
        task_lifecycle.put(
          task_lifecycle.new(),
          task_lifecycle.Validating(
            task_ref: task_ref,
            issue: issue,
            generation: generation,
          ),
        )
      directory
    },
    next_dispatch_validation_generation: generation + 1,
  )
}

fn state_with_retry(
  issue: tracker_issue.Issue,
  generation: Int,
) -> transition_types.State {
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      claimed: dict.from_list([#(task_identity(issue), issue.identifier)]),
      retry_attempts: dict.from_list([
        #(
          task_identity(issue),
          orchestrator_state.RetryEntry(
            task_ref: task.from_legacy_issue(issue).ref,
            issue_id: issue.id,
            delay_ms: 1000,
            timer_generation: generation,
          ),
        ),
      ]),
    )
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    runtime: runtime,
    lifecycle: {
      let assert Ok(directory) =
        task_lifecycle.put(
          task_lifecycle.new(),
          task_lifecycle.RetryWaiting(
            task_ref: task.from_legacy_issue(issue).ref,
            issue_id: issue.id,
            generation: generation,
            delay_ms: 1000,
          ),
        )
      directory
    },
  )
}

fn state_with_worker(
  issue: tracker_issue.Issue,
  status: transition_types.WorkerStatus,
) -> transition_types.State {
  let task_value = task.from_legacy_issue(issue)
  let worker_identity = task_identity(issue)
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      running: dict.from_list([
        #(
          worker_identity,
          orchestrator_state.RunningEntry(
            task: task_value,
            issue: issue,
            workspace_path: "test/tmp/workspaces/ABC-1",
            session: None,
          ),
        ),
      ]),
      claimed: dict.from_list([#(worker_identity, issue.identifier)]),
    )
  let worker =
    transition_types.WorkerEntry(
      task_ref: task_value.ref,
      issue_id: issue.id,
      run_id: "run-1",
      session_id: "session-1",
      issue: issue,
      workspace_path: "test/tmp/workspaces/ABC-1",
      workflow_id: "default",
      command_route_id: "worker:run-1:1",
      status: status,
      recovery: None,
    )
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    runtime: runtime,
    workers: transition_types.WorkerDirectory(
      by_issue: dict.from_list([#(worker_identity, worker)]),
      by_session: dict.from_list([#("session-1", worker_identity)]),
      route_to_session: dict.from_list([#("worker:run-1:1", "session-1")]),
      yaml_step_runs: dict.new(),
      stopped_yaml_runs: dict.new(),
    ),
    lifecycle: {
      let lifecycle = case status {
        transition_types.WorkerStarting ->
          task_lifecycle.Starting(
            task_ref: task_value.ref,
            issue: issue,
            run_id: "run-1",
            session_id: "session-1",
            workspace_path: "test/tmp/workspaces/ABC-1",
          )
        transition_types.WorkerRunning ->
          task_lifecycle.Running(
            task_ref: task_value.ref,
            issue: issue,
            run_id: "run-1",
            session_id: "session-1",
            workspace_path: "test/tmp/workspaces/ABC-1",
          )
        transition_types.WorkerStopping(reason) ->
          task_lifecycle.Stopping(
            task_ref: task_value.ref,
            issue: issue,
            run_id: "run-1",
            session_id: "session-1",
            workspace_path: "test/tmp/workspaces/ABC-1",
            reason: reason,
          )
        transition_types.WorkerFinishedStatus ->
          task_lifecycle.Completed(task_ref: task_value.ref, issue: issue)
      }
      let assert Ok(directory) =
        task_lifecycle.put(task_lifecycle.new(), lifecycle)
      directory
    },
  )
}

fn valid_handoff_claim(issue: tracker_issue.Issue) -> transition_types.Message {
  transition_types.HandoffClaimCompleted(
    task_identity(issue),
    identity.issue_id_from_string(issue.id),
    identity.run_id_from_string("run-1"),
    transition_types.HandoffClaimSucceeded(claim_batch(issue)),
  )
}

fn valid_retry_handoff_claim(
  issue: tracker_issue.Issue,
) -> transition_types.Message {
  let run_id = retry_run_id(issue)
  transition_types.HandoffClaimCompleted(
    task_identity(issue),
    identity.issue_id_from_string(issue.id),
    identity.run_id_from_string(run_id),
    transition_types.HandoffClaimSucceeded(retry_claim_batch(issue, run_id)),
  )
}

fn task_identity(issue: tracker_issue.Issue) -> identity.TaskIdentity {
  orchestrator_state.issue_identity(issue)
}

fn claim_batch(issue: tracker_issue.Issue) -> ledger_batch.LedgerBatch {
  claim_batch_with_run_id(issue, "run-1")
}

fn retry_claim_batch(
  issue: tracker_issue.Issue,
  run_id: String,
) -> ledger_batch.LedgerBatch {
  claim_batch_with_run_id(issue, run_id)
  |> ledger_batch.append_retry_cancelled(issue.id, 3, "retry_dispatch")
}

fn claim_batch_with_run_id(
  issue: tracker_issue.Issue,
  run_id: String,
) -> ledger_batch.LedgerBatch {
  ledger_batch.claim_started(
    record.WorkflowRunStartedWithTask(
      run_id,
      "default",
      "workflow-fingerprint",
      issue.id,
      issue.identifier,
      record.linear_task_ref_fields(issue.id, Some(issue.identifier), None),
      "issue-fingerprint",
      123,
      "test/tmp/workspaces/ABC-1",
    ),
    issue.id,
    issue.identifier,
    "test/tmp/workspaces/ABC-1",
    0,
    1,
    456,
  )
}

fn retry_run_id(issue: tracker_issue.Issue) -> String {
  issue.identifier <> "-123-1"
}

fn worker_success(issue: tracker_issue.Issue) -> agent_types.WorkerSuccess {
  agent_types.WorkerSuccess(
    final_issue: Some(issue),
    final_classification: agent_types.FinalTerminal,
    workspace_path: "test/tmp/workspaces/ABC-1",
    tokens: session_tokens.zero_token_totals(),
    turns: 1,
    result: result_artifact.empty(),
  )
}

fn lifecycle_context() -> transition_types.WorkerLifecycleContext {
  transition_types.WorkerLifecycleContext(
    effective: orchestrator_transition_test.fixture_effective(),
    now_ms: 456,
    secrets: [],
  )
}

fn edge_shell() -> interpreter.ShellState(List(String)) {
  interpreter.new_production_shell_state(
    data: [],
    append_ledger: fn(events, request) {
      #(append_event(events, "append:" <> request.correlation_id), Ok(Nil))
    },
    now_ms: fn(_) { 456 },
    log_effect: fn(events, _, event, _) {
      append_event(events, "log:" <> event)
    },
    start_worker: fn(events, request) {
      #(
        append_event(
          events,
          "start:"
            <> identity.issue_id_to_string(request.issue_id)
            <> ":"
            <> identity.run_id_to_string(request.run_id)
            <> ":"
            <> identity.session_id_to_string(request.session_id),
        ),
        Ok(Nil),
      )
    },
    reply_snapshot: fn(events, _) { append_event(events, "snapshot") },
    mark_poll_in_flight: fn(events, generation) {
      append_event(events, "poll:" <> int.to_string(generation))
    },
    schedule_next_poll: fn(events) { append_event(events, "poll:next") },
    fetch_candidates: fn(events, generation) {
      append_event(events, "fetch:" <> int.to_string(generation))
    },
    begin_dispatch_validation: fn(events, issue_id, _) {
      append_event(events, "validate:" <> issue_id)
    },
    begin_review_lane_preflight: fn(events, request) {
      append_event(events, "preflight:" <> request.issue_id)
    },
    reserve_session_sequence: fn(events, sequence) {
      append_event(events, "reserve:" <> int.to_string(sequence))
    },
    claim_issue: fn(events, _, issue, _, _) {
      append_event(events, "claim:" <> issue.id)
    },
    report_invalid_workflow: fn(events, issue, _, _, _) {
      append_event(events, "invalid:" <> issue.id)
    },
    remove_retry_timer: fn(events, issue_id) {
      append_event(events, "retry:remove:" <> issue_id)
    },
    finish_retry_refresh: fn(events, issue_id) {
      append_event(events, "retry:finish:" <> issue_id)
    },
    defer_retry_timer: fn(events, issue_id, generation, delay_ms) {
      append_event(
        events,
        "retry:defer:"
          <> issue_id
          <> ":"
          <> int.to_string(generation)
          <> ":"
          <> int.to_string(delay_ms),
      )
    },
    begin_retry_refresh: fn(events, issue_id, generation) {
      append_event(
        events,
        "retry:begin:" <> issue_id <> ":" <> int.to_string(generation),
      )
    },
    schedule_retry_timer: fn(events, issue_id, delay_ms, generation, _) {
      append_event(
        events,
        "retry:schedule:"
          <> issue_id
          <> ":"
          <> int.to_string(delay_ms)
          <> ":"
          <> int.to_string(generation),
      )
    },
    schedule_recovered_retry_timer: fn(events, issue_id, delay_ms, generation) {
      append_event(
        events,
        "retry:recovered:"
          <> issue_id
          <> ":"
          <> int.to_string(delay_ms)
          <> ":"
          <> int.to_string(generation),
      )
    },
    cancel_retry_timer: fn(events, issue_id, generation, cancel_reason) {
      append_event(
        events,
        "retry:cancel:"
          <> issue_id
          <> ":"
          <> int.to_string(generation)
          <> ":"
          <> cancel_reason,
      )
    },
    release_claim: fn(events, issue_id) {
      append_event(events, "release:" <> issue_id)
    },
    clear_recovery: fn(events, issue_id) {
      append_event(events, "clear_recovery:" <> issue_id)
    },
    worker_start_failed: fn(events, request, reason) {
      append_event(
        events,
        "worker_start_failed:"
          <> identity.run_id_to_string(request.run_id)
          <> ":"
          <> reason,
      )
    },
    remove_worker: fn(events, worker_identity, _) {
      append_event(
        events,
        "remove:" <> identity.issue_id_to_string(worker_identity.issue_id),
      )
    },
    publish_worker_exited: fn(events, request) {
      append_event(
        events,
        "publish:" <> identity.issue_id_to_string(request.identity.issue_id),
      )
    },
    report_worker_success: fn(events, worker_identity, _) {
      append_event(
        events,
        "success:" <> identity.issue_id_to_string(worker_identity.issue_id),
      )
    },
    report_worker_failure: fn(events, worker_identity, _) {
      append_event(
        events,
        "failure:" <> identity.issue_id_to_string(worker_identity.issue_id),
      )
    },
    cleanup_workspace: fn(events, path) {
      append_event(events, "cleanup:" <> path)
    },
    park_issue: fn(events, parked, _) {
      append_event(events, "park:" <> parked.issue_id)
    },
    report_park: fn(events, report) {
      append_event(events, "report_park:" <> report.task.remote_id)
    },
    stop_worker: fn(events, worker_identity, _) {
      append_event(
        events,
        "stop:" <> identity.issue_id_to_string(worker_identity.issue_id),
      )
    },
    stop_worker_after_issue_refresh: fn(events, worker_identity, _) {
      append_event(
        events,
        "stop_refresh:" <> identity.issue_id_to_string(worker_identity.issue_id),
      )
    },
    register_yaml_step_started: fn(events, session_id, _) {
      append_event(
        events,
        "yaml:start:" <> identity.session_id_to_string(session_id),
      )
    },
    finish_yaml_step_route: fn(events, session_id) {
      append_event(
        events,
        "yaml:route:" <> identity.session_id_to_string(session_id),
      )
    },
    finish_yaml_step_session: fn(events, session_id, _) {
      append_event(
        events,
        "yaml:session:" <> identity.session_id_to_string(session_id),
      )
    },
    finish_yaml_step_sessions_for_run: fn(events, run_id, _) {
      append_event(
        events,
        "yaml:sessions_for_run:" <> identity.run_id_to_string(run_id),
      )
    },
    clear_yaml_step_routes_for_run: fn(events, run_id) {
      append_event(events, "yaml:clear:" <> identity.run_id_to_string(run_id))
    },
    mark_yaml_run_stopping: fn(events, run_id, _) {
      append_event(
        events,
        "yaml:stopping:" <> identity.run_id_to_string(run_id),
      )
    },
    shutdown_runtime: fn(events, stop_effect_runner) {
      append_event(events, "shutdown:" <> bool_string(stop_effect_runner))
    },
    set_operator_paused: fn(events, paused) {
      append_event(events, "paused:" <> bool_string(paused))
    },
    apply_operator_command: fn(events, request) {
      #(
        append_event(events, "operator:apply"),
        command.rejected(request.operator_command, "unhandled", None),
      )
    },
    finish_operator_command: fn(events, _, result) {
      #(append_event(events, "operator:finish:" <> result.command), [])
    },
    report_park_effect: fn(events, issue_id, _, _, _, _) {
      append_event(events, "park:report:" <> issue_id)
    },
  )
}

fn append_event(events: List(String), event: String) -> List(String) {
  list.append(events, [event])
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}
