import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import orchestrator_transition_test
import scherzo/agent/types as agent_types
import scherzo/handoff
import scherzo/orchestrator/effects/interpreter
import scherzo/orchestrator/state as orchestrator_state
import scherzo/orchestrator/transition_runner
import scherzo/orchestrator/transition_types
import scherzo/result_artifact
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger
import scherzo/state/record
import scherzo/state/recovery
import scherzo/tracker/issue as tracker_issue

pub fn transition_runner_applies_effects_and_follow_ups_in_order_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)
  let shell = event_shell()

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state,
      shell: shell,
      messages: [claim_ledger_append_requested()],
      max_messages: 8,
    )

  assert exhausted == False
  assert interpreter.data(shell)
    == ["append:claim:issue-1:run-1", "start:run-1"]
  assert dict.get(next.pending_claims, "issue-1") == Error(Nil)
  assert dict.get(next.runtime.running, "issue-1")
    == Ok(orchestrator_state.RunningEntry(
      issue: issue,
      workspace_path: "test/tmp/workspaces/ABC-1",
      session: None,
    ))
  assert dict.get(next.runtime.claimed, "issue-1") == Ok("ABC-1")
}

pub fn transition_runner_applies_snapshot_reply_effect_test() {
  let state = orchestrator_transition_test.fixture_state()
  let shell = event_shell()

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state,
      shell: shell,
      messages: [transition_types.SnapshotRequested],
      max_messages: 4,
    )

  assert exhausted == False
  assert next == state
  assert interpreter.data(shell) == ["snapshot"]
}

pub fn transition_runner_retry_continue_regardless_keeps_timer_after_append_failure_test() {
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      claimed: dict.from_list([#("issue-1", "ABC-1")]),
      retry_attempts: dict.from_list([
        #(
          "issue-1",
          orchestrator_state.RetryEntry(
            issue_id: "issue-1",
            delay_ms: 10_000,
            timer_generation: 1,
          ),
        ),
      ]),
    )
  let state =
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      runtime: runtime,
    )
  let shell = append_failure_shell()

  let transition_runner.RunResult(shell: shell, exhausted: exhausted, ..) =
    transition_runner.run(
      state: state,
      shell: shell,
      messages: [
        transition_types.RetryRefreshCompleted(
          "issue-1",
          1,
          Error("api"),
          orchestrator_transition_test.fixture_context(),
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  assert interpreter.data(shell)
    == [
      "retry:finish:issue-1",
      "append:retry_cancel:issue-1:2",
      "retry:cancel:issue-1",
      "append:retry_schedule:issue-1:2",
      "retry:schedule:issue-1",
    ]
}

pub fn transition_runner_stops_at_message_limit_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)
  let shell = event_shell()

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state,
      shell: shell,
      messages: [claim_ledger_append_requested()],
      max_messages: 1,
    )

  assert exhausted == True
  assert interpreter.data(shell) == ["append:claim:issue-1:run-1"]
  assert next == state
}

pub fn worker_start_success_registers_worker_directory_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state,
      shell: event_shell(),
      messages: [claim_ledger_append_requested()],
      max_messages: 8,
    )

  assert exhausted == False
  assert dict.get(next.workers.by_issue, issue.id)
    == Ok(transition_types.WorkerEntry(
      issue_id: issue.id,
      run_id: "run-1",
      session_id: "session-1",
      issue: issue,
      workspace_path: "test/tmp/workspaces/ABC-1",
      workflow_id: "default",
      command_route_id: "worker:run-1:1",
      status: transition_types.WorkerRunning,
      recovery: None,
    ))
  assert dict.get(next.workers.by_session, "session-1") == Ok(issue.id)
  assert dict.get(next.workers.route_to_session, "worker:run-1:1")
    == Ok("session-1")
  assert interpreter.data(shell)
    == ["append:claim:issue-1:run-1", "start:run-1"]
}

pub fn worker_start_failure_clears_runtime_and_route_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state,
      shell: start_failure_shell(),
      messages: [claim_ledger_append_requested()],
      max_messages: 8,
    )

  assert exhausted == False
  assert dict.get(next.pending_claims, issue.id) == Error(Nil)
  assert dict.get(next.runtime.running, issue.id) == Error(Nil)
  assert dict.get(next.runtime.claimed, issue.id) == Error(Nil)
  assert dict.get(next.workers.by_issue, issue.id) == Error(Nil)
  assert dict.get(next.workers.route_to_session, "worker:run-1:1") == Error(Nil)
  assert interpreter.data(shell)
    == [
      "append:claim:issue-1:run-1",
      "start:run-1",
      "start_failed:spawn_failed",
      "log:worker_start_failed",
    ]
}

pub fn worker_finish_removes_running_and_reports_success_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_running_worker(issue)
  let success =
    agent_types.WorkerSuccess(
      final_issue: Some(issue),
      final_classification: agent_types.FinalTerminal,
      workspace_path: "test/tmp/workspaces/ABC-1",
      tokens: session_tokens.zero_token_totals(),
      turns: 1,
      result: result_artifact.empty(),
    )

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state,
      shell: event_shell(),
      messages: [
        transition_types.WorkerFinished(
          issue.id,
          "run-1",
          transition_types.WorkerSucceeded(success),
          lifecycle_context(),
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  assert dict.get(next.runtime.running, issue.id) == Error(Nil)
  assert dict.get(next.workers.by_issue, issue.id) == Error(Nil)
  assert dict.get(next.runtime.completed, issue.id) == Ok(issue)
  assert interpreter.data(shell)
    == [
      "remove:issue-1",
      "log:worker_exited",
      "publish:issue-1",
      "append:worker_finish:issue-1:run-1",
      "success:issue-1",
      "release:issue-1",
    ]
}

pub fn worker_down_known_removes_worker_and_reports_failure_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_running_worker(issue)

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state,
      shell: event_shell(),
      messages: [
        transition_types.WorkerDown(
          transition_types.KnownWorkerDown(issue.id, "run-1", "session-1"),
          lifecycle_context(),
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  assert dict.get(next.runtime.running, issue.id) == Error(Nil)
  assert dict.get(next.workers.by_issue, issue.id) == Error(Nil)
  assert dict.get(next.runtime.retry_attempts, issue.id)
    == Ok(orchestrator_state.RetryEntry(
      issue_id: issue.id,
      delay_ms: 10_000,
      timer_generation: 1,
    ))
  assert interpreter.data(shell)
    == [
      "log:worker_down",
      "remove:issue-1",
      "log:worker_exited",
      "publish:issue-1",
      "append:worker_failure:issue-1:run-1",
      "failure:issue-1",
      "append:retry_cancel:issue-1:1",
      "retry:cancel:issue-1",
      "append:retry_schedule:issue-1:1",
      "retry:schedule:issue-1",
    ]
}

pub fn worker_down_stale_is_safe_test() {
  let state = orchestrator_transition_test.fixture_state()

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state,
      shell: event_shell(),
      messages: [
        transition_types.WorkerDown(
          transition_types.WorkerDownStale("issue-1"),
          lifecycle_context(),
        ),
      ],
      max_messages: 4,
    )

  assert exhausted == False
  assert next == state
  assert interpreter.data(shell) == ["log:worker_down_stale"]
}

pub fn yaml_step_cleanup_removes_pure_route_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_running_worker(issue)

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state,
      shell: event_shell(),
      messages: [
        transition_types.YamlStepStarted("step-session", "run-1"),
        transition_types.YamlStepFinished("step-session"),
      ],
      max_messages: 4,
    )

  assert exhausted == False
  assert dict.get(next.workers.yaml_step_runs, "step-session") == Error(Nil)
  assert interpreter.data(shell)
    == ["yaml_start:step-session", "yaml_finish:step-session"]
}

pub fn startup_recovery_schedules_retry_cleanup_ack_and_park_effects_test() {
  let state = orchestrator_transition_test.fixture_state()

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state,
      shell: event_shell(),
      messages: [
        transition_types.StartupRecoveryApplied(
          retry_timers: [
            recovery.RecoveredRetry("issue-1", "ABC-1", 250, 3, "failure"),
          ],
          cleanup_workspaces: [
            recovery.CleanupRequest(
              "issue-2",
              "ABC-2",
              "test/tmp/workspaces/ABC-2",
            ),
          ],
          outbox_to_replay: [
            recovery.OutboxReplay(
              "outbox-1",
              "issue-3",
              "linear_command_ack",
              "linear_command_ack:comment-1",
              "{\"type\":\"linear_command_ack\",\"source_comment_id\":\"comment-1\",\"body\":\"ack\"}",
            ),
          ],
          park_reports: [
            handoff.ParkReport(
              issue_id: "issue-4",
              issue_identifier: "ABC-4",
              reason: "max_retry_attempts",
              release_policy: Some("explicit_unpark_only"),
              run_id: Some("run-9"),
            ),
          ],
          warnings: ["warn me"],
          secrets: [],
        ),
      ],
      max_messages: 4,
    )

  assert exhausted == False
  assert next == state
  assert interpreter.data(shell)
    == [
      "log:workflow_recovery_status",
      "log:recovered_retry_scheduled",
      "recovered_retry:issue-1:3",
      "log:workflow_recovery_status",
      "log:recovered_workspace_cleanup",
      "cleanup:test/tmp/workspaces/ABC-2",
      "log:outbox_replay_enqueued",
      "ack:issue-3:comment-1:ack",
      "report_park:issue-4",
      "log:workflow_recovery_status",
      "log:startup_recovery_warning",
    ]
}

pub fn shutdown_effect_is_interpreted_by_shell_test() {
  let issue = orchestrator_transition_test.fixture_issue()

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: state_with_running_worker(issue),
      shell: event_shell(),
      messages: [transition_types.ShutdownRequested(True)],
      max_messages: 4,
    )

  assert exhausted == False
  assert dict.get(next.runtime.running, issue.id) == Error(Nil)
  assert dict.get(next.runtime.claimed, issue.id) == Error(Nil)
  assert dict.get(next.workers.by_issue, issue.id) == Error(Nil)
  assert interpreter.data(shell) == ["shutdown:True"]
}

fn state_with_running_worker(
  issue: tracker_issue.Issue,
) -> transition_types.State {
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      running: dict.from_list([
        #(
          issue.id,
          orchestrator_state.RunningEntry(
            issue: issue,
            workspace_path: "test/tmp/workspaces/ABC-1",
            session: None,
          ),
        ),
      ]),
      claimed: dict.from_list([#(issue.id, issue.identifier)]),
    )
  let entry =
    transition_types.WorkerEntry(
      issue_id: issue.id,
      run_id: "run-1",
      session_id: "session-1",
      issue: issue,
      workspace_path: "test/tmp/workspaces/ABC-1",
      workflow_id: "default",
      command_route_id: "worker:run-1:1",
      status: transition_types.WorkerRunning,
      recovery: None,
    )
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    runtime: runtime,
    workers: transition_types.WorkerDirectory(
      by_issue: dict.from_list([#(issue.id, entry)]),
      by_session: dict.from_list([#("session-1", issue.id)]),
      route_to_session: dict.from_list([#("worker:run-1:1", "session-1")]),
      yaml_step_runs: dict.new(),
      stopped_yaml_runs: dict.new(),
    ),
  )
}

fn lifecycle_context() -> transition_types.WorkerLifecycleContext {
  transition_types.WorkerLifecycleContext(
    effective: orchestrator_transition_test.fixture_effective(),
    now_ms: 456,
  )
}

fn event_shell() -> interpreter.ShellState(List(String)) {
  shell_with_append_and_start_result(Ok(Nil), Ok(Nil))
}

fn append_failure_shell() -> interpreter.ShellState(List(String)) {
  shell_with_append_and_start_result(Error(ledger.Io("disk full")), Ok(Nil))
}

fn start_failure_shell() -> interpreter.ShellState(List(String)) {
  shell_with_append_and_start_result(Ok(Nil), Error("spawn_failed"))
}

fn shell_with_append_and_start_result(
  append_result: Result(Nil, ledger.LedgerError),
  start_result: Result(Nil, String),
) -> interpreter.ShellState(List(String)) {
  interpreter.new_production_shell_state(
    data: [],
    append_ledger: fn(events, request) {
      #(
        list.append(events, ["append:" <> request.correlation_id]),
        append_result,
      )
    },
    now_ms: fn(_) { 456 },
    log_effect: fn(events, _, event, _) {
      list.append(events, ["log:" <> event])
    },
    start_worker: fn(events, request) {
      #(list.append(events, ["start:" <> request.run_id]), start_result)
    },
    reply_snapshot: fn(events, _) { list.append(events, ["snapshot"]) },
    mark_poll_in_flight: fn(events, generation) {
      list.append(events, ["poll:" <> int.to_string(generation)])
    },
    schedule_next_poll: fn(events) { list.append(events, ["poll:next"]) },
    fetch_candidates: fn(events, generation) {
      list.append(events, ["fetch:" <> int.to_string(generation)])
    },
    fetch_linear_commands: fn(events, _, _, _, _) { events },
    begin_dispatch_validation: fn(events, issue_id, _) {
      list.append(events, ["validate:" <> issue_id])
    },
    reserve_session_sequence: fn(events, sequence) {
      list.append(events, ["reserve:" <> int.to_string(sequence)])
    },
    claim_issue: fn(events, issue, _, _) {
      list.append(events, ["claim:" <> issue.id])
    },
    report_invalid_workflow: fn(events, issue, _, _, _) {
      list.append(events, ["invalid:" <> issue.id])
    },
    remove_retry_timer: fn(events, issue_id) {
      list.append(events, ["retry:remove:" <> issue_id])
    },
    finish_retry_refresh: fn(events, issue_id) {
      list.append(events, ["retry:finish:" <> issue_id])
    },
    defer_retry_timer: fn(events, issue_id, _, _) {
      list.append(events, ["retry:defer:" <> issue_id])
    },
    begin_retry_refresh: fn(events, issue_id, _) {
      list.append(events, ["retry:refresh:" <> issue_id])
    },
    schedule_retry_timer: fn(events, issue_id, _, _, _) {
      list.append(events, ["retry:schedule:" <> issue_id])
    },
    schedule_recovered_retry_timer: fn(events, issue_id, _, generation) {
      list.append(events, [
        "recovered_retry:" <> issue_id <> ":" <> int.to_string(generation),
      ])
    },
    cancel_retry_timer: fn(events, issue_id, _, _) {
      list.append(events, ["retry:cancel:" <> issue_id])
    },
    release_claim: fn(events, issue_id) {
      list.append(events, ["release:" <> issue_id])
    },
    clear_recovery: fn(events, issue_id) {
      list.append(events, ["clear_recovery:" <> issue_id])
    },
    worker_start_failed: fn(events, _, reason) {
      list.append(events, ["start_failed:" <> reason])
    },
    remove_worker: fn(events, identity, _) {
      list.append(events, ["remove:" <> identity.issue_id])
    },
    publish_worker_exited: fn(events, request) {
      list.append(events, ["publish:" <> request.identity.issue_id])
    },
    report_worker_success: fn(events, identity, _) {
      list.append(events, ["success:" <> identity.issue_id])
    },
    report_worker_failure: fn(events, identity, _) {
      list.append(events, ["failure:" <> identity.issue_id])
    },
    cleanup_workspace: fn(events, path) {
      list.append(events, ["cleanup:" <> path])
    },
    park_issue: fn(events, parked, _) {
      list.append(events, ["park:" <> parked.issue_id])
    },
    replay_linear_command_ack: fn(events, issue_id, source_comment_id, body) {
      list.append(events, [
        "ack:" <> issue_id <> ":" <> source_comment_id <> ":" <> body,
      ])
    },
    report_park: fn(events, report) {
      list.append(events, ["report_park:" <> report.issue_id])
    },
    stop_worker: fn(events, identity, _) {
      list.append(events, ["stop:" <> identity.issue_id])
    },
    register_yaml_step_started: fn(events, session_id, _) {
      list.append(events, ["yaml_start:" <> session_id])
    },
    finish_yaml_step_route: fn(events, session_id) {
      list.append(events, ["yaml_finish:" <> session_id])
    },
    finish_yaml_step_session: fn(events, session_id, _) {
      list.append(events, ["yaml_session_finish:" <> session_id])
    },
    finish_yaml_step_sessions_for_run: fn(events, run_id, _) {
      list.append(events, ["yaml_run_finish:" <> run_id])
    },
    clear_yaml_step_routes_for_run: fn(events, run_id) {
      list.append(events, ["yaml_clear:" <> run_id])
    },
    mark_yaml_run_stopping: fn(events, run_id, _) {
      list.append(events, ["yaml_stop:" <> run_id])
    },
    shutdown_runtime: fn(events, stop_effect_runner) {
      list.append(events, ["shutdown:" <> bool.to_string(stop_effect_runner)])
    },
  )
}

fn claim_ledger_append_requested() -> transition_types.Message {
  transition_types.ClaimLedgerAppendRequested(
    correlation_id: "claim:issue-1:run-1",
    issue_id: "issue-1",
    run_id: "run-1",
    session_id: "session-1",
    bodies: [
      record.RunStarted(
        "run-1",
        "issue-1",
        "ABC-1",
        "test/tmp/workspaces/ABC-1",
      ),
    ],
    failure_event: "ledger_append_failed",
  )
}
