import gleam/dict
import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import gleam/string
import orchestrator_transition_test
import scherzo/agent/types as agent_types
import scherzo/orchestrator/daemon_capabilities
import scherzo/orchestrator/scheduled_runtime
import scherzo/orchestrator/transition_types
import scherzo/orchestrator/worker_lifecycle
import scherzo/orchestrator/worker_registry
import scherzo/result_artifact
import scherzo/runtime/identity
import scherzo/runtime_bundle
import scherzo/session/tokens as session_tokens
import scherzo/task
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_dag
import scherzo/workflow_run

type TestState {
  TestState(
    registry: worker_registry.Registry,
    events: List(String),
    report_requests: Int,
    action_batches: Int,
    pending_starts: Int,
  )
}

type NeedsHumanCall {
  NeedsHumanFailureLedger(String, Bool, Option(String))
  NeedsHumanReportRequest(scheduled_runtime.FailureReportRequest)
}

fn append_event(state: TestState, event: String) -> TestState {
  TestState(..state, events: [event, ..state.events])
}

fn test_clock(now_ms: Int) -> daemon_capabilities.Clock {
  daemon_capabilities.clock(fn() { now_ms })
}

fn test_logger() -> daemon_capabilities.Logger {
  daemon_capabilities.logger(fn(_, _, _, _) { Ok(Nil) })
}

fn test_events() -> daemon_capabilities.EventPublisher {
  daemon_capabilities.event_publisher(process.new_subject(), fn() { 123 })
}

fn test_ledger(event: String) -> daemon_capabilities.LedgerWriter(TestState) {
  daemon_capabilities.ledger_writer(
    append_bodies: fn(state, _, _) { #(append_event(state, event), True) },
    append_bodies_best_effort: fn(state, _, _) { append_event(state, event) },
    append_records: fn(state, _, _) { #(state, Ok(Nil)) },
  )
}

fn test_capabilities(
  now_ms: Int,
  ledger_event: String,
) -> daemon_capabilities.DaemonCapabilities(TestState, Nil, Nil) {
  daemon_capabilities.daemon_capabilities(
    clock: test_clock(now_ms),
    logger: test_logger(),
    events: test_events(),
    ledger: test_ledger(ledger_event),
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

fn issue(id: String, identifier: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
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

fn worker_handle() -> worker_registry.WorkerHandle {
  let item = issue("1", "ABC-1")
  worker_registry.WorkerHandle(
    task_ref: task.from_legacy_issue(item).ref,
    issue_id: item.id,
    issue: item,
    run_id: "run-1",
    pid: process.self(),
    monitor: process.monitor(process.self()),
    workspace_path: "workspace/ABC-1",
    session_id: "session-1",
    command_subject: None,
  )
}

fn scheduled_handle() -> worker_registry.ScheduledWorkerHandle {
  worker_registry.ScheduledWorkerHandle(
    job_id: "repair",
    workflow_id: "repair",
    due_at_ms: 1,
    run_id: "run-1",
    pid: process.self(),
    monitor: process.monitor(process.self()),
    run_root: "workspace/repair",
    session_id: "session-scheduled",
    attempt: 1,
    command_subject: None,
  )
}

fn pending_start() -> scheduled_runtime.PendingStart {
  scheduled_runtime.PendingStart(
    job_id: "repair",
    workflow_id: "repair",
    due_at_ms: 1,
    run_id: "run-1",
    trigger: "timer",
    requested_at_ms: 1,
    attempt: 2,
    blocking_reason: "",
  )
}

fn workflow_success(
  classification: agent_types.FinalClassification,
) -> workflow_run.WorkflowRunSuccess {
  workflow_success_with_tokens(
    classification,
    session_tokens.zero_token_totals(),
  )
}

fn workflow_success_with_tokens(
  classification: agent_types.FinalClassification,
  tokens: session_tokens.TokenTotals,
) -> workflow_run.WorkflowRunSuccess {
  workflow_run.WorkflowRunSuccess(
    worker_success: agent_types.WorkerSuccess(
      final_issue: None,
      final_classification: classification,
      workspace_path: "workspace/repair",
      tokens: tokens,
      turns: 1,
      result: result_artifact.empty(),
    ),
    artifacts: dict.new(),
    run_root: "workspace/repair",
    cleanup_warning: None,
  )
}

fn workflow_failure() -> workflow_run.WorkflowRunFailure {
  workflow_run.WorkflowRunFailure(
    reason: "boom",
    agent_reason: None,
    artifacts: dict.new(),
    run_root: None,
    failed_step_id: None,
  )
}

fn receive_needs_human_call(
  subject: process.Subject(NeedsHumanCall),
) -> NeedsHumanCall {
  let assert Ok(call) = process.receive(subject, within: 100)
  call
}

pub fn worker_lifecycle_rebuilds_missing_workflow_snapshot_for_start_test() {
  let orchestrator = orchestrator_transition_test.fixture_orchestrator()
  let bundle =
    runtime_bundle.RuntimeBundle(
      config_path: "scherzo.yaml",
      config_contents: "",
      dependencies: [],
      effective: orchestrator.effective,
      orchestrator: orchestrator,
      workflows: dict.from_list([
        #(
          "default",
          orchestrator_transition_test.fixture_workflow_dag("default"),
        ),
      ]),
      secrets: [],
    )

  let assert Ok(snapshot) =
    worker_lifecycle.workflow_snapshot_for_start(
      None,
      bundle,
      issue("1", "ABC-1"),
      "default",
      "run-1",
    )

  assert workflow_dag.id(worker_lifecycle.workflow_snapshot_dag(snapshot))
    == "default"
  assert worker_lifecycle.workflow_snapshot_fingerprint(snapshot) != ""
  assert string.contains(
    worker_lifecycle.workflow_snapshot_run_root(snapshot),
    "default/ABC-1/run-1",
  )
}

pub fn worker_lifecycle_handle_worker_command_ready_registers_subject_test() {
  let handle = worker_handle()
  let command_subject = process.new_subject()
  let state =
    TestState(
      registry: worker_registry.new() |> worker_registry.register_worker(handle),
      events: [],
      report_requests: 0,
      action_batches: 0,
      pending_starts: 0,
    )
  let context =
    worker_lifecycle.WorkerCommandReadyContext(
      state: state,
      run_transition_messages: fn(state, messages) {
        assert messages
          == [
            transition_types.WorkerCommandReady(
              identity.issue_id_from_string("1"),
              identity.run_id_from_string("run-1"),
            ),
          ]
        append_event(state, "transition")
      },
      registry: fn(state) { state.registry },
      set_registry: fn(state, registry) {
        TestState(..state, registry: registry)
      },
    )

  let state =
    worker_lifecycle.handle_worker_command_ready(
      context,
      "1",
      "run-1",
      command_subject,
    )

  let assert Ok(found) = worker_registry.worker_for_issue(state.registry, "1")
  let assert Some(found_subject) = found.command_subject
  assert found_subject == command_subject
  assert state.events == ["transition"]
  process.demonitor_process(handle.monitor)
}

pub fn worker_lifecycle_spawn_scheduled_worker_threads_started_ledger_state_test() {
  let pending = pending_start()
  let state =
    TestState(
      registry: worker_registry.new(),
      events: [],
      report_requests: 0,
      action_batches: 0,
      pending_starts: 1,
    )
  let context =
    worker_lifecycle.ScheduledWorkerSpawnContext(
      state: state,
      capabilities: test_capabilities(123, "ledger_started"),
      reserve_session_sequence: fn(state) { state },
      register_session: fn(session_id, display_ref, run_root, started_at_ms) {
        assert session_id == "run-1-a2"
        assert display_ref == "scheduled-repair"
        assert run_root == "workspace/repair"
        assert started_at_ms == 123
      },
      spawn: fn(started_at_ms, session_id) {
        assert started_at_ms == 123
        assert session_id == "run-1-a2"
        process.self()
      },
      register_scheduled_worker: fn(state, handle) {
        assert state.events == ["ledger_started"]
        TestState(
          ..state,
          registry: worker_registry.register_scheduled_worker(
            state.registry,
            handle,
          ),
        )
      },
      remove_pending_start: fn(state, job_id) {
        assert job_id == "repair"
        assert state.events == ["ledger_started"]
        TestState(..append_event(state, "pending_removed"), pending_starts: 0)
      },
    )

  let state =
    worker_lifecycle.spawn_scheduled_worker(
      context,
      pending,
      "workspace/repair",
    )

  let assert Ok(found) =
    worker_registry.scheduled_worker_for_run(state.registry, "run-1")
  assert found.workflow_id == "repair"
  assert found.session_id == "run-1-a2"
  assert state.events == ["pending_removed", "ledger_started"]
  assert state.pending_starts == 0
  process.demonitor_process(found.monitor)
}

pub fn worker_lifecycle_finish_scheduled_worker_success_terminal_test() {
  let handle = scheduled_handle()
  let state =
    TestState(
      registry: worker_registry.new(),
      events: [],
      report_requests: 0,
      action_batches: 0,
      pending_starts: 0,
    )
  let context =
    worker_lifecycle.ScheduledWorkerSuccessContext(
      state: state,
      capabilities: test_capabilities(123, "ledger"),
      needs_human: fn(state, _, _) { append_event(state, "needs_human") },
    )

  let state =
    worker_lifecycle.finish_scheduled_worker_success(
      context,
      handle,
      workflow_success(agent_types.FinalTerminal),
    )

  assert state.events == ["ledger"]
  process.demonitor_process(handle.monitor)
}

pub fn worker_lifecycle_finish_scheduled_worker_success_delegates_needs_human_test() {
  let handle = scheduled_handle()
  let state =
    TestState(
      registry: worker_registry.new(),
      events: [],
      report_requests: 0,
      action_batches: 0,
      pending_starts: 0,
    )
  let context =
    worker_lifecycle.ScheduledWorkerSuccessContext(
      state: state,
      capabilities: test_capabilities(123, "ledger"),
      needs_human: fn(state, _, _) { append_event(state, "needs_human") },
    )

  let state =
    worker_lifecycle.finish_scheduled_worker_success(
      context,
      handle,
      workflow_success(agent_types.FinalActive),
    )

  assert state.events == ["needs_human"]
  process.demonitor_process(handle.monitor)
}

pub fn worker_lifecycle_finish_scheduled_worker_needs_human_reports_side_effects_test() {
  let handle = scheduled_handle()
  let calls = process.new_subject()
  let tokens =
    session_tokens.TokenTotals(
      input: 1,
      output: 2,
      cache_read: 3,
      cache_write: 4,
      total: 10,
    )
  let state =
    TestState(
      registry: worker_registry.new(),
      events: [],
      report_requests: 0,
      action_batches: 0,
      pending_starts: 0,
    )
  let context =
    worker_lifecycle.ScheduledWorkerNeedsHumanContext(
      state: state,
      capabilities: test_capabilities(123, "ledger"),
      append_failure_ledger: fn(state, _, reason, retry_exhausted, run_root) {
        process.send(
          calls,
          NeedsHumanFailureLedger(reason, retry_exhausted, run_root),
        )
        state
      },
      begin_failure_report_request: fn(state, request) {
        process.send(calls, NeedsHumanReportRequest(request))
        TestState(..state, report_requests: state.report_requests + 1)
      },
    )

  let state =
    worker_lifecycle.finish_scheduled_worker_needs_human(
      context,
      handle,
      workflow_success_with_tokens(agent_types.FinalActive, tokens),
    )

  let assert NeedsHumanFailureLedger(
    "needs_human",
    True,
    Some("workspace/repair"),
  ) = receive_needs_human_call(calls)
  let assert NeedsHumanReportRequest(request) = receive_needs_human_call(calls)
  assert request
    == scheduled_runtime.FailureReportRequest(
      job_id: "repair",
      workflow_id: "repair",
      due_at_ms: 1,
      run_id: "run-1",
      attempt: 1,
      reason: "needs_human",
      run_root: Some("workspace/repair"),
      session_id: Some("session-scheduled"),
    )
  assert state.report_requests == 1
  process.demonitor_process(handle.monitor)
}

pub fn worker_lifecycle_finish_scheduled_worker_failure_reports_test() {
  let handle = scheduled_handle()
  let state =
    TestState(
      registry: worker_registry.new(),
      events: [],
      report_requests: 0,
      action_batches: 0,
      pending_starts: 0,
    )
  let context =
    worker_lifecycle.ScheduledWorkerFailureContext(
      state: state,
      capabilities: test_capabilities(123, "ledger"),
      worker_failure_follow_up: fn(state, _, _, run_root) {
        #(
          state,
          scheduled_runtime.WorkerFailureReport(
            scheduled_runtime.FailureReportRequest(
              job_id: "repair",
              workflow_id: "repair",
              due_at_ms: 1,
              run_id: "run-1",
              attempt: 1,
              reason: "boom",
              run_root: run_root,
              session_id: Some("session-scheduled"),
            ),
          ),
        )
      },
      append_failure_ledger: fn(state, _, _, _, _) { state },
      begin_failure_report_request: fn(state, _) {
        TestState(..state, report_requests: state.report_requests + 1)
      },
    )

  let state =
    worker_lifecycle.finish_scheduled_worker_failure(
      context,
      handle,
      workflow_failure(),
    )

  assert state.report_requests == 1
  assert state.action_batches == 0
  process.demonitor_process(handle.monitor)
}

pub fn worker_lifecycle_finish_scheduled_worker_failure_has_no_retry_branch_test() {
  let handle = scheduled_handle()
  let state =
    TestState(
      registry: worker_registry.new(),
      events: [],
      report_requests: 0,
      action_batches: 0,
      pending_starts: 0,
    )
  let context =
    worker_lifecycle.ScheduledWorkerFailureContext(
      state: state,
      capabilities: test_capabilities(123, "ledger"),
      worker_failure_follow_up: fn(state, _, _, run_root) {
        #(
          state,
          scheduled_runtime.WorkerFailureReport(
            scheduled_runtime.FailureReportRequest(
              job_id: "repair",
              workflow_id: "repair",
              due_at_ms: 1,
              run_id: "run-1",
              attempt: 1,
              reason: "boom",
              run_root: run_root,
              session_id: Some("session-scheduled"),
            ),
          ),
        )
      },
      append_failure_ledger: fn(state, _, _, _, _) {
        append_event(state, "ledger")
      },
      begin_failure_report_request: fn(state, _) {
        TestState(..state, report_requests: state.report_requests + 1)
      },
    )

  let state =
    worker_lifecycle.finish_scheduled_worker_failure(
      context,
      handle,
      workflow_failure(),
    )

  assert state.report_requests == 1
  assert state.action_batches == 0
  process.demonitor_process(handle.monitor)
}

pub fn worker_lifecycle_scheduled_worker_down_starts_pending_after_report_test() {
  let handle = scheduled_handle()
  let state =
    TestState(
      registry: worker_registry.new(),
      events: [],
      report_requests: 0,
      action_batches: 0,
      pending_starts: 0,
    )
  let context =
    worker_lifecycle.ScheduledWorkerDownContext(
      state: state,
      capabilities: test_capabilities(123, "ledger"),
      set_registry: fn(state, registry) {
        TestState(..state, registry: registry)
      },
      worker_failure_follow_up: fn(state, _, _, run_root) {
        #(
          state,
          scheduled_runtime.WorkerFailureReport(
            scheduled_runtime.FailureReportRequest(
              job_id: "repair",
              workflow_id: "repair",
              due_at_ms: 1,
              run_id: "run-1",
              attempt: 1,
              reason: "worker_down",
              run_root: run_root,
              session_id: Some("session-scheduled"),
            ),
          ),
        )
      },
      begin_failure_report_request: fn(state, _) {
        TestState(..state, report_requests: state.report_requests + 1)
      },
      start_pending_scheduled_runs: fn(state) {
        TestState(..state, pending_starts: state.pending_starts + 1)
      },
    )

  let registry = worker_registry.new()
  let state =
    worker_lifecycle.scheduled_worker_down(
      context,
      registry,
      handle.run_id,
      handle,
    )

  assert state.report_requests == 1
  assert state.action_batches == 0
  assert state.pending_starts == 1
  process.demonitor_process(handle.monitor)
}
