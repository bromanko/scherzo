import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import orchestrator_transition_test
import scherzo/agent/types as agent_types
import scherzo/control/command
import scherzo/error
import scherzo/orchestrator/daemon_capabilities
import scherzo/orchestrator/daemon_transition_shell
import scherzo/orchestrator/effects/types as effects_types
import scherzo/orchestrator/transition_invariants
import scherzo/orchestrator/transition_types
import scherzo/result_artifact
import scherzo/runtime/identity
import scherzo/runtime/reason as orchestrator_reason
import scherzo/runtime/state as orchestrator_state
import scherzo/session/reason as session_reason
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger
import scherzo/state/ledger_batch
import scherzo/state/record
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/workflow_policy

pub fn run_applies_effects_and_merges_transition_state_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state =
    shell_state(orchestrator_transition_test.state_with_pending_claim(issue))

  let next =
    daemon_transition_shell.run(context(state, 8), [
      handoff_claim_succeeded(),
    ])

  assert next.events == ["append:ledger_append_failed", "start:run-1"]
  let identity = orchestrator_state.issue_identity(issue)
  assert dict.get(next.transition_state.pending_claims, identity) == Error(Nil)
  assert dict.get(next.transition_state.runtime.running, identity)
    == Ok(orchestrator_state.RunningEntry(
      task: task.from_legacy_issue(issue),
      issue: issue,
      workspace_path: "test/tmp/workspaces/ABC-1",
      session: None,
    ))
}

pub fn run_empty_messages_preserve_transition_state_test() {
  let original = orchestrator_transition_test.fixture_state()
  let state = shell_state(original)

  let next = daemon_transition_shell.run(context(state, 8), [])

  assert next.events == []
  assert next.transition_state == original
}

pub fn run_logs_exhaustion_with_configured_limit_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let original = orchestrator_transition_test.state_with_pending_claim(issue)
  let state = shell_state(original)

  let next =
    daemon_transition_shell.run(context(state, 1), [
      handoff_claim_succeeded(),
    ])

  assert next.events == ["append:ledger_append_failed"]
  assert next.transition_state == original
  assert next.exhausted_limits == [1]
}

pub fn run_logs_and_marks_fatal_invariant_violations_test() {
  let state = shell_state(state_with_claimed_lifecycle_missing())

  let next = daemon_transition_shell.run(context(state, 8), [])

  assert next.events == ["invariants:fail:claimed_lifecycle_missing"]
}

pub fn run_warn_mode_does_not_mark_invariant_violations_fatal_test() {
  let state = shell_state(state_with_claimed_lifecycle_missing())

  let next =
    daemon_transition_shell.run(
      context_with_invariant_mode(
        state,
        8,
        daemon_transition_shell.WarnOnInvariantViolation,
      ),
      [],
    )

  assert next.events == []
}

pub fn run_treats_slot_overcommit_as_warning_in_fail_mode_test() {
  let state = shell_state(state_with_pending_slot_overcommit())

  let next = daemon_transition_shell.run(context(state, 8), [])

  assert next.events == []
}

pub fn run_caps_invariant_violation_log_payload_test() {
  let state = shell_state(orchestrator_transition_test.fixture_state())

  let next =
    daemon_transition_shell.run(
      context_with_invariant_checker(
        state,
        8,
        daemon_transition_shell.FailOnInvariantViolation,
        fn(_) { Error(invariant_errors(40)) },
      ),
      [],
    )

  assert list.any(next.events, fn(event) {
    string.starts_with(event, "invariants:fail:")
    && string.contains(event, "rule-40")
  })
}

pub fn run_one_message_with_operator_reply_uses_finish_hook_test() {
  let transition_state = orchestrator_transition_test.fixture_state()
  let state = shell_state(transition_state)
  let request = retry_request("missing-issue")
  let next =
    daemon_transition_shell.run_one_message_with_operator_reply(
      context: context(state, daemon_transition_shell.default_message_limit()),
      message: operator_message(request),
      operator_command: request.operator_command,
      send_reply: fn(_) { Nil },
    )

  assert next.events == ["finish_operator:retry:rejected", "snapshot"]
}

pub fn run_one_message_with_operator_reply_logs_exhaustion_test() {
  let transition_state = orchestrator_transition_test.fixture_state()
  let state = shell_state(transition_state)
  let request = retry_request("missing-issue")
  let next =
    daemon_transition_shell.run_one_message_with_operator_reply(
      context: context(state, 0),
      message: operator_message(request),
      operator_command: request.operator_command,
      send_reply: fn(_) { Nil },
    )

  assert next.events == ["finish_operator:retry:rejected"]
  assert next.exhausted_limits == [0]
}

pub fn run_one_message_with_operator_reply_preserves_finish_hook_core_updates_test() {
  let transition_state = orchestrator_transition_test.fixture_state()
  let state = shell_state(transition_state)
  let request = retry_request("missing-issue")
  let next =
    daemon_transition_shell.run_one_message_with_operator_reply(
      context: context(
        ShellState(..state, finish_hook_updates_pending_claim: True),
        daemon_transition_shell.default_message_limit(),
      ),
      message: operator_message(request),
      operator_command: request.operator_command,
      send_reply: fn(_) { Nil },
    )

  assert next.events == ["finish_operator:retry:rejected", "snapshot"]
  assert dict.get(
      next.transition_state.pending_claims,
      orchestrator_state.linear_issue_id_identity("issue-1"),
    )
    == Ok(finish_hook_pending_claim())
}

pub fn interpret_effects_covers_callback_surface_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let transition_state = orchestrator_transition_test.fixture_state()
  let parked =
    orchestrator_state.ParkedEntry(
      task_ref: task.from_legacy_issue(issue).ref,
      issue_id: issue.id,
      identifier: issue.identifier,
      reason: orchestrator_reason.ParkOperator("operator_hold"),
      release_policy: orchestrator_state.ExplicitUnparkOnly,
      parked_at_ms: 123,
    )
  let worker_start_request = worker_start(issue)
  let worker_identity_value = worker_identity(issue)
  let apply_request = operator_request(command.PauseDispatch)
  let finish_request = operator_request(command.ResumeDispatch)

  let #(next, follow_up_messages) =
    daemon_transition_shell.interpret_effects(
      shell_state(transition_state),
      handlers(),
      [
        effects_types.Log("info", "custom_event", []),
        effects_types.AppendLedger(effects_types.LedgerAppend(
          correlation_id: "direct",
          batch: ledger_batch.retry_cancelled("issue-1", 1, "test"),
          failure_event: "direct_failed",
          policy: effects_types.ContinueRegardless,
        )),
        effects_types.StartWorker(worker_start_request),
        effects_types.ReplySnapshot(transition_state.runtime),
        effects_types.MarkPollInFlight(4),
        effects_types.ScheduleNextPoll,
        effects_types.FetchCandidates(5),
        effects_types.BeginDispatchValidation(issue.id, 6),
        effects_types.ReserveSessionSequence(7),
        effects_types.ClaimIssue(
          task_ref: task.from_legacy_issue(issue).ref,
          issue: issue,
          workspace_path: "test/tmp/workspaces/ABC-1",
          run_id: "run-1",
          remaining_candidates: [],
        ),
        effects_types.ReportInvalidWorkflow(
          issue: issue,
          violation: workflow_policy.MissingWorkflowLabel,
          violation_fingerprint: "missing_workflow_label",
          reporting_policy_fingerprint: "policy",
        ),
        effects_types.RemoveRetryTimer(issue.id),
        effects_types.FinishRetryRefresh(issue.id),
        effects_types.DeferRetryTimer(issue.id, generation: 8, delay_ms: 500),
        effects_types.BeginRetryRefresh(issue.id, 9),
        effects_types.ScheduleRetryTimer(
          issue_id: issue.id,
          delay_ms: 600,
          generation: 10,
          retry_reason: orchestrator_reason.RetryAfterFailure,
        ),
        effects_types.ScheduleRecoveredRetryTimer(
          issue_id: issue.id,
          delay_ms: 700,
          generation: 11,
        ),
        effects_types.CancelRetryTimer(
          issue_id: issue.id,
          generation: 12,
          cancel_reason: "operator_cancel",
        ),
        effects_types.ReleaseClaim(issue.id),
        effects_types.ClearRecovery(issue.id),
        effects_types.WorkerStartFailed(worker_start_request, "spawn failed"),
        effects_types.RemoveWorker(worker_identity_value, True),
        effects_types.PublishWorkerExited(worker_exit_publication(
          worker_identity_value,
        )),
        effects_types.ReportWorkerSuccess(
          worker_identity_value,
          worker_success(issue),
        ),
        effects_types.ReportWorkerFailure(
          worker_identity_value,
          worker_failure(),
        ),
        effects_types.CleanupWorkspace("test/tmp/workspaces/ABC-1"),
        effects_types.ParkIssue(parked, Some("run-1")),
        effects_types.ReportPark(park_report()),
        effects_types.StopWorker(
          worker_identity_value,
          session_reason.OperatorAbort,
        ),
        effects_types.StopWorkerAfterIssueRefresh(
          worker_identity_value,
          orchestrator_reason.StopTerminal,
        ),
        effects_types.RegisterYamlStepStarted(
          identity.session_id_from_string("session-1"),
          identity.run_id_from_string("run-1"),
        ),
        effects_types.FinishYamlStepRoute(identity.session_id_from_string(
          "session-1",
        )),
        effects_types.FinishYamlStepSession(
          identity.session_id_from_string("session-1"),
          session_reason.Normal,
        ),
        effects_types.FinishYamlStepSessionsForRun(
          identity.run_id_from_string("run-1"),
          session_reason.Failed,
        ),
        effects_types.ClearYamlStepRoutesForRun(identity.run_id_from_string(
          "run-1",
        )),
        effects_types.MarkYamlRunStopping(
          identity.run_id_from_string("run-1"),
          session_reason.Stopped,
        ),
        effects_types.ShutdownRuntime(True),
        effects_types.SetOperatorPaused(True),
        effects_types.ApplyOperatorCommand(apply_request),
        effects_types.FinishOperatorCommand(
          finish_request,
          command.queued(command.ResumeDispatch, None),
        ),
        effects_types.ReportParkEffect(
          issue_id: issue.id,
          issue_identifier: issue.identifier,
          reason: "waiting_on_input",
          release_policy: "manual",
          source_run_id: Some("run-1"),
        ),
      ],
    )

  assert next.events
    == [
      "append:direct_failed",
      "start:run-1",
      "snapshot",
      "poll_in_flight:4",
      "schedule_poll",
      "fetch:5",
      "validate:issue-1",
      "reserve:7",
      "claim:issue-1",
      "invalid:issue-1",
      "retry_remove:issue-1",
      "retry_finish:issue-1",
      "retry_defer:issue-1:8:500",
      "retry_begin:issue-1:9",
      "retry_schedule:issue-1:600:10",
      "retry_recovered:issue-1:700:11",
      "retry_cancel:issue-1:12:operator_cancel",
      "release:issue-1",
      "clear_recovery:issue-1",
      "worker_start_failed:run-1:spawn failed",
      "remove_worker:session-1:True",
      "publish_worker_exited:session-1",
      "worker_success:session-1",
      "worker_failure:session-1",
      "cleanup:test/tmp/workspaces/ABC-1",
      "park:issue-1",
      "report_park:issue-1",
      "stop_worker:session-1",
      "stop_worker_after_refresh:session-1",
      "yaml_start:session-1",
      "yaml_route:session-1",
      "yaml_session:session-1",
      "yaml_sessions_for_run:run-1",
      "yaml_clear:run-1",
      "yaml_stopping:run-1",
      "shutdown:True",
      "paused:True",
      "apply_operator:pause",
      "finish_operator:resume:queued",
      "report_park_effect:issue-1",
    ]
  assert follow_up_messages
    == [
      transition_types.WorkerStartSucceeded(
        identity.issue_id_from_string("issue-1"),
        identity.run_id_from_string("run-1"),
        identity.session_id_from_string("session-1"),
      ),
      transition_types.OperatorCommandCompleted(
        apply_request,
        command.applied(apply_request.operator_command, None),
      ),
      transition_types.SnapshotRequested,
    ]
}

pub fn interpret_effects_surfaces_ledger_append_failure_test() {
  let request =
    effects_types.LedgerAppend(
      correlation_id: "claim:issue-1:run-1",
      batch: ledger_batch.retry_cancelled("issue-1", 1, "test"),
      failure_event: "ledger_append_failed",
      policy: effects_types.SpawnClaimedWorkerAfterAppend(
        task_identity: orchestrator_state.linear_issue_id_identity("issue-1"),
        issue_id: identity.issue_id_from_string("issue-1"),
        run_id: identity.run_id_from_string("run-1"),
        session_id: identity.session_id_from_string("session-1"),
      ),
    )

  let #(next, follow_up_messages) =
    daemon_transition_shell.interpret_effects(
      shell_state(orchestrator_transition_test.fixture_state()),
      failing_handlers(),
      [effects_types.AppendLedger(request)],
    )

  assert next.events == ["append_failed:ledger_append_failed"]
  assert follow_up_messages
    == [
      transition_types.LedgerAppendCompleted(
        correlation_id: "claim:issue-1:run-1",
        continuation: transition_types.SpawnClaimedWorkerAfterAppend(
          task_identity: orchestrator_state.linear_issue_id_identity("issue-1"),
          issue_id: identity.issue_id_from_string("issue-1"),
          run_id: identity.run_id_from_string("run-1"),
          session_id: identity.session_id_from_string("session-1"),
        ),
        result: Error(ledger.Io("disk full")),
        now_ms: 456,
      ),
    ]
}

type ShellState {
  ShellState(
    transition_state: transition_types.State,
    events: List(String),
    exhausted_limits: List(Int),
    finish_hook_updates_pending_claim: Bool,
  )
}

fn shell_state(transition_state: transition_types.State) -> ShellState {
  ShellState(
    transition_state: transition_state,
    events: [],
    exhausted_limits: [],
    finish_hook_updates_pending_claim: False,
  )
}

fn state_with_claimed_lifecycle_missing() -> transition_types.State {
  let issue = orchestrator_transition_test.fixture_issue()
  let task_identity = orchestrator_state.issue_identity(issue)
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    runtime: orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      claimed: dict.from_list([#(task_identity, issue.identifier)]),
    ),
  )
}

fn state_with_pending_slot_overcommit() -> transition_types.State {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)
  transition_types.State(
    ..state,
    runtime: orchestrator_state.RuntimeState(
      ..state.runtime,
      max_concurrent_agents: 0,
    ),
  )
}

fn invariant_errors(count: Int) -> List(transition_invariants.InvariantError) {
  invariant_errors_loop(1, count, [])
}

fn invariant_errors_loop(
  index: Int,
  count: Int,
  acc: List(transition_invariants.InvariantError),
) -> List(transition_invariants.InvariantError) {
  case index > count {
    True -> list.reverse(acc)
    False ->
      invariant_errors_loop(index + 1, count, [
        transition_invariants.InvariantError(
          "rule-" <> int.to_string(index),
          "identity-" <> int.to_string(index),
          "message",
        ),
        ..acc
      ])
  }
}

fn context(
  state: ShellState,
  max_messages: Int,
) -> daemon_transition_shell.Context(ShellState, Nil, Nil) {
  context_with_invariant_mode(
    state,
    max_messages,
    daemon_transition_shell.FailOnInvariantViolation,
  )
}

fn context_with_invariant_mode(
  state: ShellState,
  max_messages: Int,
  invariant_mode: daemon_transition_shell.InvariantMode,
) -> daemon_transition_shell.Context(ShellState, Nil, Nil) {
  context_with_invariant_checker(
    state,
    max_messages,
    invariant_mode,
    transition_invariants.check,
  )
}

fn context_with_invariant_checker(
  state: ShellState,
  max_messages: Int,
  invariant_mode: daemon_transition_shell.InvariantMode,
  invariant_checker: daemon_transition_shell.InvariantChecker,
) -> daemon_transition_shell.Context(ShellState, Nil, Nil) {
  context_with_handlers(
    state,
    max_messages,
    invariant_mode,
    invariant_checker,
    handlers(),
  )
}

fn context_with_handlers(
  state: ShellState,
  max_messages: Int,
  invariant_mode: daemon_transition_shell.InvariantMode,
  invariant_checker: daemon_transition_shell.InvariantChecker,
  handlers: daemon_transition_shell.ShellHandlers(ShellState, Nil, Nil),
) -> daemon_transition_shell.Context(ShellState, Nil, Nil) {
  daemon_transition_shell.context(
    state: state,
    get_transition_state: fn(state) { state.transition_state },
    put_transition_state: put_transition_state,
    log_exhausted: fn(state, max_messages) {
      ShellState(
        ..state,
        exhausted_limits: list.append(state.exhausted_limits, [max_messages]),
      )
    },
    mark_invariant_failure: fn(state, errors) {
      append_event(state, "invariants:fail:" <> invariant_codes(errors))
    },
    invariant_mode: invariant_mode,
    invariant_checker: invariant_checker,
    max_messages: max_messages,
    handlers: handlers,
  )
}

fn invariant_codes(
  errors: List(transition_invariants.InvariantError),
) -> String {
  errors
  |> list.map(transition_invariants.error_code)
  |> string.join(with: ",")
}

fn test_capabilities(
  append_prefix: String,
  append_result: Result(Nil, ledger.LedgerError),
) -> daemon_capabilities.DaemonCapabilities(ShellState, Nil, Nil) {
  daemon_capabilities.daemon_capabilities(
    clock: daemon_capabilities.clock(fn() { 456 }),
    logger: daemon_capabilities.logger(fn(_, _, _, _) { Ok(Nil) }),
    events: daemon_capabilities.event_publisher(process.new_subject(), fn() {
      456
    }),
    ledger: daemon_capabilities.ledger_writer(
      append_bodies: fn(state, _, event) {
        #(append_event(state, append_prefix <> event), append_result == Ok(Nil))
      },
      append_bodies_best_effort: fn(state, _, event) {
        append_event(state, append_prefix <> event)
      },
      append_records: fn(state, _, event) {
        #(append_event(state, append_prefix <> event), append_result)
      },
    ),
    effects: daemon_capabilities.effect_queue(
      enqueue: fn(state, _) { state },
      enqueue_outbox: fn(state, _, _) { state },
      enqueue_outbox_with_attempt_count: fn(state, _, _, _) { state },
      enqueue_outbox_with_attempt_count_result: fn(state, _, _, _) {
        #(state, True)
      },
    ),
    timers: daemon_capabilities.timers(
      send_after: fn(subject, _, message) {
        process.send(subject, message)
        Nil
      },
      cancel_timer: fn(_) { Nil },
    ),
  )
}

fn handlers() -> daemon_transition_shell.ShellHandlers(ShellState, Nil, Nil) {
  daemon_transition_shell.shell_handlers(
    capabilities: test_capabilities("append:", Ok(Nil)),
    mark_ledger_append_failed: fn(state) { state },
    start_worker: fn(state, request) {
      #(
        append_event(
          state,
          "start:" <> identity.run_id_to_string(request.run_id),
        ),
        Ok(Nil),
      )
    },
    reply_snapshot: fn(state, _) { append_event(state, "snapshot") },
    mark_poll_in_flight: fn(state, generation) {
      append_event(state, "poll_in_flight:" <> int.to_string(generation))
    },
    schedule_next_poll: fn(state) { append_event(state, "schedule_poll") },
    fetch_candidates: fn(state, generation) {
      append_event(state, "fetch:" <> int.to_string(generation))
    },
    begin_dispatch_validation: fn(state, issue_id, _) {
      append_event(state, "validate:" <> issue_id)
    },
    begin_review_lane_preflight: fn(state, request) {
      append_event(state, "preflight:" <> request.issue_id)
    },
    reserve_session_sequence: fn(state, sequence) {
      append_event(state, "reserve:" <> int.to_string(sequence))
    },
    claim_issue: fn(state, _, issue, _, _, _) {
      append_event(state, "claim:" <> issue.id)
    },
    report_invalid_workflow: fn(state, issue, _, _, _) {
      append_event(state, "invalid:" <> issue.id)
    },
    replay_outbox: fn(state, _) { state },
    remove_retry_timer: fn(state, issue_id) {
      append_event(state, "retry_remove:" <> issue_id)
    },
    finish_retry_refresh: fn(state, issue_id) {
      append_event(state, "retry_finish:" <> issue_id)
    },
    defer_retry_timer: fn(state, issue_id, generation, delay_ms) {
      append_event(
        state,
        "retry_defer:"
          <> issue_id
          <> ":"
          <> int.to_string(generation)
          <> ":"
          <> int.to_string(delay_ms),
      )
    },
    begin_retry_refresh: fn(state, issue_id, generation) {
      append_event(
        state,
        "retry_begin:" <> issue_id <> ":" <> int.to_string(generation),
      )
    },
    schedule_retry_timer: fn(state, issue_id, delay_ms, generation, _) {
      append_event(
        state,
        "retry_schedule:"
          <> issue_id
          <> ":"
          <> int.to_string(delay_ms)
          <> ":"
          <> int.to_string(generation),
      )
    },
    schedule_recovered_retry_timer: fn(state, issue_id, delay_ms, generation) {
      append_event(
        state,
        "retry_recovered:"
          <> issue_id
          <> ":"
          <> int.to_string(delay_ms)
          <> ":"
          <> int.to_string(generation),
      )
    },
    cancel_retry_timer: fn(state, issue_id, generation, cancel_reason) {
      append_event(
        state,
        "retry_cancel:"
          <> issue_id
          <> ":"
          <> int.to_string(generation)
          <> ":"
          <> cancel_reason,
      )
    },
    release_claim: fn(state, issue_id) {
      append_event(state, "release:" <> issue_id)
    },
    clear_recovery: fn(state, issue_id) {
      append_event(state, "clear_recovery:" <> issue_id)
    },
    worker_start_failed: fn(state, request, reason) {
      append_event(
        state,
        "worker_start_failed:"
          <> identity.run_id_to_string(request.run_id)
          <> ":"
          <> reason,
      )
    },
    remove_worker: fn(state, worker_identity, demonitor) {
      append_event(
        state,
        "remove_worker:"
          <> identity.session_id_to_string(worker_identity.session_id)
          <> ":"
          <> bool_string(demonitor),
      )
    },
    publish_worker_exited: fn(state, request) {
      append_event(
        state,
        "publish_worker_exited:"
          <> identity.session_id_to_string(request.identity.session_id),
      )
    },
    report_worker_success: fn(state, worker_identity, _) {
      append_event(
        state,
        "worker_success:"
          <> identity.session_id_to_string(worker_identity.session_id),
      )
    },
    report_worker_failure: fn(state, worker_identity, _) {
      append_event(
        state,
        "worker_failure:"
          <> identity.session_id_to_string(worker_identity.session_id),
      )
    },
    cleanup_workspace: fn(state, workspace_path) {
      append_event(state, "cleanup:" <> workspace_path)
    },
    park_issue: fn(state, parked, _) {
      append_event(state, "park:" <> parked.issue_id)
    },
    report_park: fn(state, report) {
      append_event(state, "report_park:" <> report.task.remote_id)
    },
    stop_worker: fn(state, worker_identity, _) {
      append_event(
        state,
        "stop_worker:"
          <> identity.session_id_to_string(worker_identity.session_id),
      )
    },
    stop_worker_after_issue_refresh: fn(state, worker_identity, _) {
      append_event(
        state,
        "stop_worker_after_refresh:"
          <> identity.session_id_to_string(worker_identity.session_id),
      )
    },
    register_yaml_step_started: fn(state, session_id, _) {
      append_event(
        state,
        "yaml_start:" <> identity.session_id_to_string(session_id),
      )
    },
    finish_yaml_step_route: fn(state, session_id) {
      append_event(
        state,
        "yaml_route:" <> identity.session_id_to_string(session_id),
      )
    },
    finish_yaml_step_session: fn(state, session_id, _) {
      append_event(
        state,
        "yaml_session:" <> identity.session_id_to_string(session_id),
      )
    },
    finish_yaml_step_sessions_for_run: fn(state, run_id, _) {
      append_event(
        state,
        "yaml_sessions_for_run:" <> identity.run_id_to_string(run_id),
      )
    },
    clear_yaml_step_routes_for_run: fn(state, run_id) {
      append_event(state, "yaml_clear:" <> identity.run_id_to_string(run_id))
    },
    mark_yaml_run_stopping: fn(state, run_id, _) {
      append_event(state, "yaml_stopping:" <> identity.run_id_to_string(run_id))
    },
    shutdown_runtime: fn(state, stop_effect_runner) {
      append_event(state, "shutdown:" <> bool_string(stop_effect_runner))
    },
    set_operator_paused: fn(state, paused) {
      append_event(state, "paused:" <> bool_string(paused))
    },
    apply_operator_command: fn(state, request) {
      #(
        append_event(
          state,
          "apply_operator:" <> command.command_name(request.operator_command),
        ),
        command.applied(request.operator_command, None),
        [],
      )
    },
    finish_operator_command: fn(state, request, result) {
      let state =
        append_event(
          state,
          "finish_operator:"
            <> command.command_name(request.operator_command)
            <> ":"
            <> command.status_to_string(result.status),
        )
      let state = case state.finish_hook_updates_pending_claim {
        True ->
          put_transition_state(
            state,
            transition_types.State(
              ..state.transition_state,
              pending_claims: dict.insert(
                state.transition_state.pending_claims,
                orchestrator_state.linear_issue_id_identity("issue-1"),
                finish_hook_pending_claim(),
              ),
            ),
          )
        False -> state
      }
      #(state, [transition_types.SnapshotRequested])
    },
    report_park_effect: fn(state, issue_id, _, _, _, _) {
      append_event(state, "report_park_effect:" <> issue_id)
    },
  )
}

fn failing_handlers() -> daemon_transition_shell.ShellHandlers(
  ShellState,
  Nil,
  Nil,
) {
  daemon_transition_shell.shell_handlers(
    capabilities: test_capabilities(
      "append_failed:",
      Error(ledger.Io("disk full")),
    ),
    mark_ledger_append_failed: fn(state) { state },
    start_worker: fn(state, request) {
      #(
        append_event(
          state,
          "start:" <> identity.run_id_to_string(request.run_id),
        ),
        Ok(Nil),
      )
    },
    reply_snapshot: fn(state, _) { state },
    mark_poll_in_flight: fn(state, _) { state },
    schedule_next_poll: fn(state) { state },
    fetch_candidates: fn(state, generation) {
      append_event(state, "fetch:" <> int.to_string(generation))
    },
    begin_dispatch_validation: fn(state, issue_id, _) {
      append_event(state, "validate:" <> issue_id)
    },
    begin_review_lane_preflight: fn(state, request) {
      append_event(state, "preflight:" <> request.issue_id)
    },
    reserve_session_sequence: fn(state, _) { state },
    claim_issue: fn(state, _, issue, _, _, _) {
      append_event(state, "claim:" <> issue.id)
    },
    report_invalid_workflow: fn(state, issue, _, _, _) {
      append_event(state, "invalid:" <> issue.id)
    },
    replay_outbox: fn(state, _) { state },
    remove_retry_timer: fn(state, _) { state },
    finish_retry_refresh: fn(state, issue_id) {
      append_event(state, "retry_finish:" <> issue_id)
    },
    defer_retry_timer: fn(state, _, _, _) { state },
    begin_retry_refresh: fn(state, issue_id, generation) {
      append_event(
        state,
        "retry_begin:" <> issue_id <> ":" <> int.to_string(generation),
      )
    },
    schedule_retry_timer: fn(state, _, _, _, _) { state },
    schedule_recovered_retry_timer: fn(state, _, _, _) { state },
    cancel_retry_timer: fn(state, _, _, _) { state },
    release_claim: fn(state, issue_id) {
      append_event(state, "release:" <> issue_id)
    },
    clear_recovery: fn(state, _) { state },
    worker_start_failed: fn(state, _, _) { state },
    remove_worker: fn(state, _, _) { state },
    publish_worker_exited: fn(state, _) { state },
    report_worker_success: fn(state, _, _) { state },
    report_worker_failure: fn(state, _, _) { state },
    cleanup_workspace: fn(state, workspace_path) {
      append_event(state, "cleanup:" <> workspace_path)
    },
    park_issue: fn(state, parked, _) {
      append_event(state, "park:" <> parked.issue_id)
    },
    report_park: fn(state, report) {
      append_event(state, "report_park:" <> report.task.remote_id)
    },
    stop_worker: fn(state, _, _) { state },
    stop_worker_after_issue_refresh: fn(state, _, _) { state },
    register_yaml_step_started: fn(state, session_id, _) {
      append_event(
        state,
        "yaml_start:" <> identity.session_id_to_string(session_id),
      )
    },
    finish_yaml_step_route: fn(state, _) { state },
    finish_yaml_step_session: fn(state, _, _) { state },
    finish_yaml_step_sessions_for_run: fn(state, _, _) { state },
    clear_yaml_step_routes_for_run: fn(state, _) { state },
    mark_yaml_run_stopping: fn(state, _, _) { state },
    shutdown_runtime: fn(state, stop_effect_runner) {
      append_event(state, "shutdown:" <> bool_string(stop_effect_runner))
    },
    set_operator_paused: fn(state, paused) {
      append_event(state, "paused:" <> bool_string(paused))
    },
    apply_operator_command: fn(state, request) {
      #(
        append_event(
          state,
          "apply_operator:" <> command.command_name(request.operator_command),
        ),
        command.applied(request.operator_command, None),
        [],
      )
    },
    finish_operator_command: fn(state, request, result) {
      #(
        append_event(
          state,
          "finish_operator:"
            <> command.command_name(request.operator_command)
            <> ":"
            <> command.status_to_string(result.status),
        ),
        [transition_types.SnapshotRequested],
      )
    },
    report_park_effect: fn(state, issue_id, _, _, _, _) {
      append_event(state, "report_park_effect:" <> issue_id)
    },
  )
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "True"
    False -> "False"
  }
}

fn park_report() -> adapter.ParkReport {
  adapter.ParkReport(
    task: task.TaskRef(
      backend_kind: "linear",
      remote_id: "issue-1",
      key: None,
      url: None,
    ),
    issue_identifier: "ABC-1",
    reason: "waiting_on_input",
    release_policy: Some("manual"),
    run_id: Some("run-1"),
  )
}

fn operator_request(
  operator_command: command.OperatorCommand,
) -> effects_types.OperatorCommandRequest {
  effects_types.OperatorCommandRequest(
    correlation_id: "test-correlation",
    source: effects_types.LocalOperatorCommand,
    operator_command: operator_command,
    timeout_ms: 1000,
  )
}

fn retry_request(issue_id: String) -> effects_types.OperatorCommandRequest {
  operator_request(command.RetryIssue(command.IssueId(issue_id)))
}

fn operator_message(
  request: effects_types.OperatorCommandRequest,
) -> transition_types.Message {
  transition_types.OperatorCommandSubmitted(
    request: request,
    context: orchestrator_transition_test.fixture_context(),
    issue_resolution: transition_types.OperatorIssueNotResolved,
    parked_issue_resolution: transition_types.ParkedIssueNotResolved,
  )
}

fn worker_start(issue: tracker_issue.Issue) -> effects_types.WorkerStart {
  effects_types.WorkerStart(
    task_ref: task.from_legacy_issue(issue).ref,
    issue_id: identity.issue_id_from_string(issue.id),
    run_id: identity.run_id_from_string("run-1"),
    session_id: identity.session_id_from_string("session-1"),
    command_route_id: "route-1",
    issue: issue,
    workspace_path: "test/tmp/workspaces/ABC-1",
    workflow_id: "default",
    workflow_snapshot: None,
    route_label: "default",
    recovery: None,
  )
}

fn worker_identity(issue: tracker_issue.Issue) -> effects_types.WorkerIdentity {
  effects_types.WorkerIdentity(
    task_ref: task.from_legacy_issue(issue).ref,
    issue_id: identity.issue_id_from_string(issue.id),
    run_id: identity.run_id_from_string("run-1"),
    session_id: identity.session_id_from_string("session-1"),
    issue: issue,
    workspace_path: "test/tmp/workspaces/ABC-1",
    workflow_id: "default",
    command_route_id: "route-1",
  )
}

fn worker_exit_publication(
  identity: effects_types.WorkerIdentity,
) -> effects_types.WorkerExitPublication {
  effects_types.WorkerExitPublication(
    identity: identity,
    reason_text: "completed",
    exit_reason: session_reason.Normal,
    tokens: session_tokens.zero_token_totals(),
    update_tokens: True,
  )
}

fn worker_success(issue: tracker_issue.Issue) -> agent_types.WorkerSuccess {
  agent_types.WorkerSuccess(
    final_issue: Some(issue),
    final_classification: agent_types.FinalTerminal,
    workspace_path: "test/tmp/workspaces/ABC-1",
    tokens: session_tokens.zero_token_totals(),
    turns: 1,
    result: result_artifact.from_final_response(Some("ok"), False, "test"),
  )
}

fn worker_failure() -> agent_types.WorkerFailure {
  agent_types.WorkerFailure(
    reason: error.OperatorAbort,
    workspace_path: Some("test/tmp/workspaces/ABC-1"),
    tokens: session_tokens.zero_token_totals(),
    final_issue: None,
  )
}

fn append_event(state: ShellState, event: String) -> ShellState {
  ShellState(..state, events: list.append(state.events, [event]))
}

fn put_transition_state(
  state: ShellState,
  transition_state: transition_types.State,
) -> ShellState {
  ShellState(..state, transition_state: transition_state)
}

fn finish_hook_pending_claim() -> transition_types.PendingClaim {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)
  let assert Ok(pending_claim) =
    dict.get(
      state.pending_claims,
      orchestrator_state.linear_issue_id_identity("issue-1"),
    )
  pending_claim
}

fn handoff_claim_succeeded() -> transition_types.Message {
  transition_types.HandoffClaimCompleted(
    task_identity: orchestrator_state.linear_issue_id_identity("issue-1"),
    issue_id: identity.issue_id_from_string("issue-1"),
    run_id: identity.run_id_from_string("run-1"),
    result: transition_types.HandoffClaimSucceeded(ledger_batch.claim_started(
      record.WorkflowRunStartedWithTask(
        "run-1",
        "default",
        "workflow-fingerprint",
        "issue-1",
        "ABC-1",
        record.linear_task_ref_fields("issue-1", Some("ABC-1"), None),
        "issue-fingerprint",
        123,
        "test/tmp/workspaces/ABC-1",
      ),
      "issue-1",
      "ABC-1",
      "test/tmp/workspaces/ABC-1",
      0,
      1,
      456,
    )),
  )
}
