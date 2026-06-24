import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import orchestrator_transition_invariant_helpers as invariant_helpers
import orchestrator_transition_test
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/error
import scherzo/orchestrator/effects/interpreter
import scherzo/orchestrator/transition_runner
import scherzo/orchestrator/transition_types
import scherzo/result_artifact
import scherzo/runtime/identity
import scherzo/runtime/state as orchestrator_state
import scherzo/session/reason as session_reason
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger
import scherzo/state/ledger_batch
import scherzo/state/record
import scherzo/state/recovery
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt

pub fn transition_runner_applies_effects_and_follow_ups_in_order_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = orchestrator_transition_test.state_with_pending_claim(issue)
  let shell = event_shell()

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    invariant_helpers.run_and_assert(
      state: state,
      shell: shell,
      messages: [handoff_claim_succeeded()],
      max_messages: 8,
    )

  assert exhausted == False
  assert interpreter.data(shell)
    == ["append:claim:issue-1:run-1", "start:run-1"]
  let identity = orchestrator_state.issue_identity(issue)
  assert dict.get(next.pending_claims, identity) == Error(Nil)
  assert dict.get(next.runtime.running, identity)
    == Ok(orchestrator_state.RunningEntry(
      task: task.from_legacy_issue(issue),
      issue: issue,
      workspace_path: "test/tmp/workspaces/ABC-1",
      session: None,
    ))
  assert dict.get(next.runtime.claimed, identity) == Ok("ABC-1")
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

pub fn handoff_claim_stale_does_not_spawn_worker_test() {
  let issue = orchestrator_transition_test.fixture_issue()

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    transition_runner.run(
      state: orchestrator_transition_test.fixture_state(),
      shell: event_shell(),
      messages: [
        transition_types.HandoffClaimCompleted(
          orchestrator_state.issue_identity(issue),
          identity.issue_id_from_string(issue.id),
          identity.run_id_from_string("run-1"),
          transition_types.HandoffClaimFailed("stale"),
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  assert interpreter.data(shell) == ["log:handoff_claim_stale"]
  let identity = orchestrator_state.issue_identity(issue)
  assert dict.get(next.runtime.running, identity) == Error(Nil)
  assert dict.get(next.workers.by_issue, identity) == Error(Nil)
}

pub fn transition_runner_retry_continue_regardless_keeps_timer_after_append_failure_test() {
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      claimed: dict.from_list([
        #(orchestrator_state.linear_issue_id_identity("issue-1"), "ABC-1"),
      ]),
      retry_attempts: dict.from_list([
        #(
          orchestrator_state.linear_issue_id_identity("issue-1"),
          orchestrator_state.RetryEntry(
            task_ref: orchestrator_state.linear_issue_id_ref("issue-1"),
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
      "log:retry_refresh_failed",
      "append:retry_schedule:issue-1:2",
      "log:ledger_append_failed",
      "retry:defer:issue-1",
    ]
}

pub fn running_refresh_releases_stale_context_slot_before_candidate_fetch_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let terminal_issue =
    tracker_issue.Issue(
      ..issue,
      state: issue_state.from_string_unchecked("Done"),
    )
  let effective = orchestrator_transition_test.fixture_effective()
  let context =
    transition_types.DispatchContext(
      ..orchestrator_transition_test.fixture_context(),
      effective: config_types.EffectiveConfig(
        ..effective,
        agent: config_types.AgentConfig(
          ..effective.agent,
          max_concurrent_agents: 1,
        ),
      ),
      active_issue_ids: [issue.id],
      active_issues: [issue],
    )

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    invariant_helpers.run_and_assert(
      state: state_with_running_worker(issue),
      shell: event_shell(),
      messages: [
        transition_types.RunningRefreshCompleted(
          1,
          transition_types.PollSnapshot(1, Some(1)),
          Ok([terminal_issue]),
          context,
        ),
      ],
      max_messages: 8,
    )

  let identity = orchestrator_state.issue_identity(issue)
  assert exhausted == False
  assert dict.get(next.runtime.running, identity) == Error(Nil)
  assert dict.get(next.workers.by_issue, identity) == Error(Nil)
  assert interpreter.data(shell)
    == [
      "append:workflow_cancelled_issue_reconcile:issue-1:run-1:terminal",
      "stop_refresh:issue-1",
      "cleanup:test/tmp/workspaces/ABC-1",
      "fetch:1",
    ]
}

pub fn running_refresh_cancelled_append_failure_blocks_reconcile_followups_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let terminal_issue =
    tracker_issue.Issue(
      ..issue,
      state: issue_state.from_string_unchecked("Done"),
    )
  let effective = orchestrator_transition_test.fixture_effective()
  let context =
    transition_types.DispatchContext(
      ..orchestrator_transition_test.fixture_context(),
      effective: config_types.EffectiveConfig(
        ..effective,
        agent: config_types.AgentConfig(
          ..effective.agent,
          max_concurrent_agents: 1,
        ),
      ),
      active_issue_ids: [issue.id],
      active_issues: [issue],
    )

  let transition_runner.RunResult(shell: shell, exhausted: exhausted, ..) =
    transition_runner.run(
      state: state_with_running_worker(issue),
      shell: append_failure_shell(),
      messages: [
        transition_types.RunningRefreshCompleted(
          1,
          transition_types.PollSnapshot(1, Some(1)),
          Ok([terminal_issue]),
          context,
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  assert interpreter.data(shell)
    == ["append:workflow_cancelled_issue_reconcile:issue-1:run-1:terminal"]
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
      messages: [handoff_claim_succeeded()],
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
      messages: [handoff_claim_succeeded()],
      max_messages: 8,
    )

  assert exhausted == False
  let identity = orchestrator_state.issue_identity(issue)
  assert dict.get(next.workers.by_issue, identity)
    == Ok(transition_types.WorkerEntry(
      task_ref: task.from_legacy_issue(issue).ref,
      issue_id: issue.id,
      run_id: "run-1",
      session_id: "session-1",
      issue: issue,
      workspace_path: "test/tmp/workspaces/ABC-1",
      workflow_id: "default",
      workflow_snapshot: Some(
        orchestrator_transition_test.fixture_workflow_snapshot(
          "default",
          issue,
          "run-1",
        ),
      ),
      command_route_id: "worker:run-1:1",
      status: transition_types.WorkerRunning,
      recovery: None,
    ))
  assert dict.get(next.workers.by_session, "session-1") == Ok(identity)
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
      messages: [handoff_claim_succeeded()],
      max_messages: 8,
    )

  let identity = orchestrator_state.issue_identity(issue)
  assert exhausted == False
  assert dict.get(next.pending_claims, identity) == Error(Nil)
  assert dict.get(next.runtime.running, identity) == Error(Nil)
  assert dict.get(next.runtime.claimed, identity) == Error(Nil)
  assert dict.get(next.workers.by_issue, identity) == Error(Nil)
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
    invariant_helpers.run_and_assert(
      state: state,
      shell: event_shell(),
      messages: [
        transition_types.WorkerFinished(
          identity.issue_id_from_string(issue.id),
          identity.run_id_from_string("run-1"),
          Ok(success),
          lifecycle_context(),
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  let identity = orchestrator_state.issue_identity(issue)
  assert dict.get(next.runtime.running, identity) == Error(Nil)
  assert dict.get(next.workers.by_issue, identity) == Error(Nil)
  let assert Ok(completed) = dict.get(next.runtime.completed, identity)
  assert orchestrator_state.completed_issue(completed) == issue
  assert interpreter.data(shell)
    == [
      "remove:issue-1",
      "append:worker_finish:issue-1:run-1",
      "log:worker_exited",
      "publish:issue-1",
      "success:issue-1",
      "release:issue-1",
    ]
}

pub fn worker_finish_append_failure_blocks_terminal_followups_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_running_worker(issue)

  let transition_runner.RunResult(shell: shell, exhausted: exhausted, ..) =
    transition_runner.run(
      state: state,
      shell: append_failure_shell(),
      messages: [
        transition_types.WorkerFinished(
          identity.issue_id_from_string(issue.id),
          identity.run_id_from_string("run-1"),
          Ok(worker_success(issue)),
          lifecycle_context(),
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  assert interpreter.data(shell)
    == ["remove:issue-1", "append:worker_finish:issue-1:run-1"]
}

pub fn recovery_validation_append_failure_blocks_publish_and_park_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let failure =
    worker_failure(
      error.PiFailed(error.PiProtocolError(
        workflow_attempt.recovery_pi_resume_validation_failed,
      )),
    )

  let transition_runner.RunResult(shell: shell, exhausted: exhausted, ..) =
    transition_runner.run(
      state: state_with_running_worker(issue),
      shell: append_failure_shell(),
      messages: [
        transition_types.WorkerFinished(
          identity.issue_id_from_string(issue.id),
          identity.run_id_from_string("run-1"),
          Error(failure),
          lifecycle_context(),
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  assert interpreter.data(shell)
    == ["remove:issue-1", "append:worker_failure:issue-1:run-1"]
}

pub fn operator_worker_failure_append_failure_blocks_publish_and_park_test() {
  let issue = orchestrator_transition_test.fixture_issue()

  let transition_runner.RunResult(shell: shell, exhausted: exhausted, ..) =
    transition_runner.run(
      state: state_with_running_worker(issue),
      shell: append_failure_shell(),
      messages: [
        transition_types.WorkerFinished(
          identity.issue_id_from_string(issue.id),
          identity.run_id_from_string("run-1"),
          Error(worker_failure(error.OperatorAbort)),
          lifecycle_context(),
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  assert interpreter.data(shell)
    == ["remove:issue-1", "append:workflow_cancelled:issue-1:run-1"]
}

pub fn worker_finish_uses_task_ref_with_duplicate_remote_ids_test() {
  let #(state, linear_issue, memory_issue, memory_ref) =
    duplicate_remote_worker_state()
  let success =
    agent_types.WorkerSuccess(
      final_issue: Some(memory_issue),
      final_classification: agent_types.FinalTerminal,
      workspace_path: "test/tmp/workspaces/MEM-1",
      tokens: session_tokens.zero_token_totals(),
      turns: 1,
      result: result_artifact.empty(),
    )

  let transition_runner.RunResult(state: next, exhausted: exhausted, ..) =
    transition_runner.run(
      state: state,
      shell: event_shell(),
      messages: [
        transition_types.WorkerFinished(
          identity.issue_id_from_string(memory_issue.id),
          identity.run_id_from_string("run-memory"),
          Ok(success),
          lifecycle_context(),
        ),
      ],
      max_messages: 8,
    )

  let linear_identity = orchestrator_state.issue_identity(linear_issue)
  let memory_identity = orchestrator_state.task_ref_identity(memory_ref)
  assert exhausted == False
  assert dict.has_key(next.runtime.running, linear_identity)
  assert dict.get(next.runtime.running, memory_identity) == Error(Nil)
  assert dict.has_key(next.workers.by_issue, linear_identity)
  assert dict.get(next.workers.by_issue, memory_identity) == Error(Nil)
  let assert Ok(completed) = dict.get(next.runtime.completed, memory_identity)
  assert orchestrator_state.completed_issue(completed) == memory_issue
  assert dict.get(next.runtime.completed, linear_identity) == Error(Nil)
}

pub fn worker_down_uses_task_ref_with_duplicate_remote_ids_test() {
  let #(state, linear_issue, memory_issue, memory_ref) =
    duplicate_remote_worker_state()

  let transition_runner.RunResult(state: next, exhausted: exhausted, ..) =
    transition_runner.run(
      state: state,
      shell: event_shell(),
      messages: [
        transition_types.WorkerDown(
          transition_types.KnownWorkerDown(
            identity.issue_id_from_string(memory_issue.id),
            identity.run_id_from_string("run-memory"),
            identity.session_id_from_string("session-memory"),
          ),
          lifecycle_context(),
        ),
      ],
      max_messages: 8,
    )

  let linear_identity = orchestrator_state.issue_identity(linear_issue)
  let memory_identity = orchestrator_state.task_ref_identity(memory_ref)
  assert exhausted == False
  assert dict.has_key(next.runtime.running, linear_identity)
  assert dict.get(next.runtime.running, memory_identity) == Error(Nil)
  assert dict.has_key(next.workers.by_issue, linear_identity)
  assert dict.get(next.workers.by_issue, memory_identity) == Error(Nil)
  assert dict.get(next.runtime.retry_attempts, memory_identity) == Error(Nil)
  assert dict.get(next.runtime.retry_attempts, linear_identity) == Error(Nil)
  let assert Ok(parked) = dict.get(next.runtime.parked, memory_identity)
  assert parked.task_ref == memory_ref
  assert parked.issue_id == memory_issue.id
}

pub fn worker_down_known_removes_worker_and_reports_failure_test() {
  let issue = orchestrator_transition_test.fixture_issue()
  let state = state_with_running_worker(issue)

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    invariant_helpers.run_and_assert(
      state: state,
      shell: event_shell(),
      messages: [
        transition_types.WorkerDown(
          transition_types.KnownWorkerDown(
            identity.issue_id_from_string(issue.id),
            identity.run_id_from_string("run-1"),
            identity.session_id_from_string("session-1"),
          ),
          lifecycle_context(),
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  let identity = orchestrator_state.issue_identity(issue)
  assert dict.get(next.runtime.running, identity) == Error(Nil)
  assert dict.get(next.workers.by_issue, identity) == Error(Nil)
  assert dict.get(next.runtime.retry_attempts, identity) == Error(Nil)
  let assert Ok(parked) = dict.get(next.runtime.parked, identity)
  assert parked.task_ref == task.from_legacy_issue(issue).ref
  assert parked.issue_id == issue.id
  assert interpreter.data(shell)
    == [
      "log:worker_down",
      "remove:issue-1",
      "append:worker_failure:issue-1:run-1",
      "log:worker_exited",
      "publish:issue-1",
      "failure:issue-1",
      "park:issue-1",
      "release:issue-1",
    ]
}

pub fn worker_down_append_failure_blocks_failure_followups_test() {
  let issue = orchestrator_transition_test.fixture_issue()

  let transition_runner.RunResult(shell: shell, exhausted: exhausted, ..) =
    transition_runner.run(
      state: state_with_running_worker(issue),
      shell: append_failure_shell(),
      messages: [
        transition_types.WorkerDown(
          transition_types.KnownWorkerDown(
            identity.issue_id_from_string(issue.id),
            identity.run_id_from_string("run-1"),
            identity.session_id_from_string("session-1"),
          ),
          lifecycle_context(),
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  assert interpreter.data(shell)
    == [
      "log:worker_down",
      "remove:issue-1",
      "append:worker_failure:issue-1:run-1",
    ]
}

pub fn worker_stop_cancelled_append_failure_blocks_stop_followups_test() {
  let issue = orchestrator_transition_test.fixture_issue()

  let transition_runner.RunResult(shell: shell, exhausted: exhausted, ..) =
    transition_runner.run(
      state: state_with_running_worker(issue),
      shell: append_failure_shell(),
      messages: [
        transition_types.WorkerStopRequested(
          identity.session_id_from_string("session-1"),
          session_reason.OperatorAbort,
          lifecycle_context(),
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  assert interpreter.data(shell) == ["append:workflow_cancelled:issue-1:run-1"]
}

pub fn worker_down_stale_is_safe_test() {
  let state = orchestrator_transition_test.fixture_state()

  let transition_runner.RunResult(
    state: next,
    shell: shell,
    exhausted: exhausted,
  ) =
    invariant_helpers.run_and_assert(
      state: state,
      shell: event_shell(),
      messages: [
        transition_types.WorkerDown(
          transition_types.WorkerDownStale(identity.issue_id_from_string(
            "issue-1",
          )),
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
        transition_types.YamlStepStarted(
          identity.session_id_from_string("step-session"),
          identity.run_id_from_string("run-1"),
        ),
        transition_types.YamlStepFinished(identity.session_id_from_string(
          "step-session",
        )),
      ],
      max_messages: 4,
    )

  assert exhausted == False
  assert dict.get(next.workers.yaml_step_runs, "step-session") == Error(Nil)
  assert interpreter.data(shell)
    == ["yaml_start:step-session", "yaml_finish:step-session"]
}

pub fn worker_down_clears_yaml_step_directory_for_run_test() {
  let issue = orchestrator_transition_test.fixture_issue()

  let transition_runner.RunResult(state: next, exhausted: exhausted, ..) =
    invariant_helpers.run_and_assert(
      state: state_with_running_worker(issue),
      shell: event_shell(),
      messages: [
        transition_types.YamlStepStarted(
          identity.session_id_from_string("step-session"),
          identity.run_id_from_string("run-1"),
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
      max_messages: 8,
    )

  assert exhausted == False
  assert dict.get(next.workers.yaml_step_runs, "step-session") == Error(Nil)
  assert dict.get(next.workers.stopped_yaml_runs, "run-1") == Error(Nil)
}

pub fn worker_stop_clears_yaml_step_directory_for_run_test() {
  let issue = orchestrator_transition_test.fixture_issue()

  let transition_runner.RunResult(state: next, exhausted: exhausted, ..) =
    invariant_helpers.run_and_assert(
      state: state_with_running_worker(issue),
      shell: event_shell(),
      messages: [
        transition_types.YamlStepStarted(
          identity.session_id_from_string("step-session"),
          identity.run_id_from_string("run-1"),
        ),
        transition_types.WorkerStopRequested(
          identity.session_id_from_string("session-1"),
          session_reason.OperatorAbort,
          lifecycle_context(),
        ),
      ],
      max_messages: 8,
    )

  assert exhausted == False
  assert dict.get(next.workers.yaml_step_runs, "step-session") == Error(Nil)
  assert dict.get(next.workers.stopped_yaml_runs, "run-1") == Error(Nil)
}

pub fn startup_recovery_schedules_retry_cleanup_and_park_effects_test() {
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
              record.linear_task_ref_fields("issue-3", Some("ABC-3"), None),
              "linear_command_ack",
              "linear_command_ack:comment-1",
              "{\"type\":\"linear_command_ack\",\"source_comment_id\":\"comment-1\",\"body\":\"ack\"}",
            ),
          ],
          park_reports: [
            adapter.ParkReport(
              task: task.TaskRef(
                backend_kind: "linear",
                remote_id: "issue-4",
                key: Some("ABC-4"),
                url: None,
              ),
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
      "log:workflow_recovery_status",
      "replay_outbox:linear_command_ack:issue-3",
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
  let identity = orchestrator_state.issue_identity(issue)
  assert dict.get(next.runtime.running, identity) == Error(Nil)
  assert dict.get(next.runtime.claimed, identity) == Error(Nil)
  assert dict.get(next.workers.by_issue, identity) == Error(Nil)
  assert interpreter.data(shell) == ["shutdown:True"]
}

fn state_with_running_worker(
  issue: tracker_issue.Issue,
) -> transition_types.State {
  let identity = orchestrator_state.issue_identity(issue)
  let task_ref = task.from_legacy_issue(issue).ref
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      running: dict.from_list([
        #(
          identity,
          orchestrator_state.RunningEntry(
            task: task.from_legacy_issue(issue),
            issue: issue,
            workspace_path: "test/tmp/workspaces/ABC-1",
            session: None,
          ),
        ),
      ]),
      claimed: dict.from_list([#(identity, issue.identifier)]),
    )
  let entry =
    transition_types.WorkerEntry(
      task_ref: task_ref,
      issue_id: issue.id,
      run_id: "run-1",
      session_id: "session-1",
      issue: issue,
      workspace_path: "test/tmp/workspaces/ABC-1",
      workflow_id: "default",
      workflow_snapshot: None,
      command_route_id: "worker:run-1:1",
      status: transition_types.WorkerRunning,
      recovery: None,
    )
  transition_types.State(
    ..orchestrator_transition_test.fixture_state(),
    runtime: runtime,
    workers: transition_types.WorkerDirectory(
      by_issue: dict.from_list([#(identity, entry)]),
      by_session: dict.from_list([#("session-1", identity)]),
      route_to_session: dict.from_list([#("worker:run-1:1", "session-1")]),
      yaml_step_runs: dict.new(),
      stopped_yaml_runs: dict.new(),
    ),
  )
}

fn duplicate_remote_worker_state() -> #(
  transition_types.State,
  tracker_issue.Issue,
  tracker_issue.Issue,
  task.TaskRef,
) {
  let base = orchestrator_transition_test.fixture_issue()
  let linear_issue =
    tracker_issue.Issue(..base, id: "shared", identifier: "ABC-1")
  let memory_issue =
    tracker_issue.Issue(..base, id: "shared", identifier: "MEM-1")
  let memory_ref =
    task.TaskRef(
      backend_kind: "test-memory",
      remote_id: "shared",
      key: Some("MEM-1"),
      url: None,
    )
  let linear_ref = task.from_legacy_issue(linear_issue).ref
  let linear_identity = orchestrator_state.task_ref_identity(linear_ref)
  let memory_identity = orchestrator_state.task_ref_identity(memory_ref)
  let memory_task =
    task.Task(..task.from_legacy_issue(memory_issue), ref: memory_ref)
  let runtime =
    orchestrator_state.RuntimeState(
      ..orchestrator_transition_test.fixture_runtime(),
      running: dict.from_list([
        #(
          linear_identity,
          orchestrator_state.RunningEntry(
            task: task.from_legacy_issue(linear_issue),
            issue: linear_issue,
            workspace_path: "test/tmp/workspaces/ABC-1",
            session: None,
          ),
        ),
        #(
          memory_identity,
          orchestrator_state.RunningEntry(
            task: memory_task,
            issue: memory_issue,
            workspace_path: "test/tmp/workspaces/MEM-1",
            session: None,
          ),
        ),
      ]),
      claimed: dict.from_list([
        #(linear_identity, linear_issue.identifier),
        #(memory_identity, memory_issue.identifier),
      ]),
    )
  let linear_entry =
    transition_types.WorkerEntry(
      task_ref: linear_ref,
      issue_id: linear_issue.id,
      run_id: "run-linear",
      session_id: "session-linear",
      issue: linear_issue,
      workspace_path: "test/tmp/workspaces/ABC-1",
      workflow_id: "default",
      workflow_snapshot: None,
      command_route_id: "worker:run-linear:1",
      status: transition_types.WorkerRunning,
      recovery: None,
    )
  let memory_entry =
    transition_types.WorkerEntry(
      task_ref: memory_ref,
      issue_id: memory_issue.id,
      run_id: "run-memory",
      session_id: "session-memory",
      issue: memory_issue,
      workspace_path: "test/tmp/workspaces/MEM-1",
      workflow_id: "default",
      workflow_snapshot: None,
      command_route_id: "worker:run-memory:1",
      status: transition_types.WorkerRunning,
      recovery: None,
    )
  #(
    transition_types.State(
      ..orchestrator_transition_test.fixture_state(),
      runtime: runtime,
      workers: transition_types.WorkerDirectory(
        by_issue: dict.from_list([
          #(linear_identity, linear_entry),
          #(memory_identity, memory_entry),
        ]),
        by_session: dict.from_list([
          #("session-linear", linear_identity),
          #("session-memory", memory_identity),
        ]),
        route_to_session: dict.from_list([
          #("worker:run-linear:1", "session-linear"),
          #("worker:run-memory:1", "session-memory"),
        ]),
        yaml_step_runs: dict.new(),
        stopped_yaml_runs: dict.new(),
      ),
    ),
    linear_issue,
    memory_issue,
    memory_ref,
  )
}

fn lifecycle_context() -> transition_types.WorkerLifecycleContext {
  transition_types.WorkerLifecycleContext(
    effective: orchestrator_transition_test.fixture_effective(),
    now_ms: 456,
    secrets: [],
  )
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

fn worker_failure(reason: error.AgentRunnerError) -> agent_types.WorkerFailure {
  agent_types.WorkerFailure(
    reason: reason,
    workspace_path: Some("test/tmp/workspaces/ABC-1"),
    tokens: session_tokens.zero_token_totals(),
    final_issue: None,
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
      #(
        list.append(events, [
          "start:" <> identity.run_id_to_string(request.run_id),
        ]),
        start_result,
      )
    },
    reply_snapshot: fn(events, _) { list.append(events, ["snapshot"]) },
    mark_poll_in_flight: fn(events, generation) {
      list.append(events, ["poll:" <> int.to_string(generation)])
    },
    schedule_next_poll: fn(events) { list.append(events, ["poll:next"]) },
    fetch_candidates: fn(events, generation) {
      list.append(events, ["fetch:" <> int.to_string(generation)])
    },
    begin_dispatch_validation: fn(events, issue_id, _) {
      list.append(events, ["validate:" <> issue_id])
    },
    begin_review_lane_preflight: fn(events, request) {
      list.append(events, ["preflight:" <> request.issue_id])
    },
    reserve_session_sequence: fn(events, sequence) {
      list.append(events, ["reserve:" <> int.to_string(sequence)])
    },
    claim_issue: fn(events, _, issue, _, _, _) {
      list.append(events, ["claim:" <> issue.id])
    },
    report_invalid_workflow: fn(events, issue, _, _, _) {
      list.append(events, ["invalid:" <> issue.id])
    },
    replay_outbox: fn(events, outbox_replay) {
      let recovery.OutboxReplay(_, task_ref, outbox_kind, _, _) = outbox_replay
      list.append(events, [
        "replay_outbox:" <> outbox_kind <> ":" <> task_ref.task_remote_id,
      ])
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
      list.append(events, [
        "remove:" <> identity.issue_id_to_string(identity.issue_id),
      ])
    },
    publish_worker_exited: fn(events, request) {
      list.append(events, [
        "publish:" <> identity.issue_id_to_string(request.identity.issue_id),
      ])
    },
    report_worker_success: fn(events, identity, _) {
      list.append(events, [
        "success:" <> identity.issue_id_to_string(identity.issue_id),
      ])
    },
    report_worker_failure: fn(events, identity, _) {
      list.append(events, [
        "failure:" <> identity.issue_id_to_string(identity.issue_id),
      ])
    },
    cleanup_workspace: fn(events, path) {
      list.append(events, ["cleanup:" <> path])
    },
    park_issue: fn(events, parked, _) {
      list.append(events, ["park:" <> parked.issue_id])
    },
    report_park: fn(events, report) {
      list.append(events, ["report_park:" <> report.task.remote_id])
    },
    stop_worker: fn(events, identity, _) {
      list.append(events, [
        "stop:" <> identity.issue_id_to_string(identity.issue_id),
      ])
    },
    stop_worker_after_issue_refresh: fn(events, identity, _) {
      list.append(events, [
        "stop_refresh:" <> identity.issue_id_to_string(identity.issue_id),
      ])
    },
    register_yaml_step_started: fn(events, session_id, _) {
      list.append(events, [
        "yaml_start:" <> identity.session_id_to_string(session_id),
      ])
    },
    finish_yaml_step_route: fn(events, session_id) {
      list.append(events, [
        "yaml_finish:" <> identity.session_id_to_string(session_id),
      ])
    },
    finish_yaml_step_session: fn(events, session_id, _) {
      list.append(events, [
        "yaml_session_finish:" <> identity.session_id_to_string(session_id),
      ])
    },
    finish_yaml_step_sessions_for_run: fn(events, run_id, _) {
      list.append(events, [
        "yaml_run_finish:" <> identity.run_id_to_string(run_id),
      ])
    },
    clear_yaml_step_routes_for_run: fn(events, run_id) {
      list.append(events, ["yaml_clear:" <> identity.run_id_to_string(run_id)])
    },
    mark_yaml_run_stopping: fn(events, run_id, _) {
      list.append(events, ["yaml_stop:" <> identity.run_id_to_string(run_id)])
    },
    shutdown_runtime: fn(events, stop_effect_runner) {
      list.append(events, ["shutdown:" <> bool.to_string(stop_effect_runner)])
    },
    set_operator_paused: fn(events, paused) {
      list.append(events, ["operator_paused:" <> bool_string(paused)])
    },
    apply_operator_command: fn(events, request) {
      #(
        list.append(events, ["operator:apply"]),
        command.rejected(request.operator_command, "unhandled", None),
        [],
      )
    },
    finish_operator_command: fn(events, _, result) {
      #(list.append(events, ["operator:finish:" <> result.command]), [])
    },
    report_park_effect: fn(events, issue_id, _, _, _, _) {
      list.append(events, ["park:report:" <> issue_id])
    },
  )
}

fn bool_string(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
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
