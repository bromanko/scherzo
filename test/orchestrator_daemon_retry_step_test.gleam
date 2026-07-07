import gleam/bit_array
import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/artifact_publication_manifest
import scherzo/artifact_publication_planner
import scherzo/artifact_publication_recording
import scherzo/artifact_repository/command_runner
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/control/protocol
import scherzo/control/query/types as query_types
import scherzo/error
import scherzo/handoff
import scherzo/hash
import scherzo/orchestrator/core
import scherzo/orchestrator/daemon
import scherzo/orchestrator/dispatch_recovery
import scherzo/orchestrator/startup_recovery
import scherzo/orchestrator/yaml_step_session
import scherzo/path
import scherzo/port
import scherzo/result_artifact
import scherzo/retry_step_validation
import scherzo/runtime/state as orchestrator_state
import scherzo/runtime_bundle
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/reason as session_reason
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/projection
import scherzo/state/record
import scherzo/state/recovery
import scherzo/step_artifact
import scherzo/task
import scherzo/tracker
import scherzo/tracker/adapter
import scherzo/tracker/adapter_legacy
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_contract
import scherzo/workflow_contract_manifest
import scherzo/workflow_dag
import scherzo/workflow_fingerprint as workflow_fingerprint_module
import scherzo/workflow_interface_snapshot
import scherzo/workflow_repair
import scherzo/workflow_run
import scherzo/workspace
import scherzo/workspace_manifest
import simplifile
import support/test_helpers
import test_async

type FetchCounterMessage {
  FetchShouldBlock(process.Subject(Bool))
  ArmFetchGate
}

pub fn retry_recovery_rejection_messages_include_next_command_table_test() {
  let cases = [
    #(
      "artifact_recovery_failed",
      retry_step_validation.operation_failure_message(
        "artifact_recovery_failed",
        Some("artifact sha mismatch"),
        "run-1",
        Some("build"),
      ),
    ),
    #(
      "workflow_drift",
      retry_step_validation.operation_failure_message(
        "workflow_drift",
        Some("workflow fingerprint drifted"),
        "run-1",
        Some("build"),
      ),
    ),
    #(
      "ambiguous_repair_step",
      retry_step_validation.operation_failure_message(
        "ambiguous_repair_step",
        Some("multiple failed or interrupted steps match; use --step"),
        "run-1",
        Some("build"),
      ),
    ),
    #(
      "issue_state_drift:terminal_state",
      retry_step_validation.operation_failure_message(
        "issue_state_drift:terminal_state",
        Some("issue is terminal"),
        "run-1",
        Some("build"),
      ),
    ),
    #(
      "issue_parked",
      "issue is parked for operator_hold; no run, park, or tracker state was changed. Next safe command: scripts/scherzoctl unpark LIV-1 --json",
    ),
    #(
      "control_operation_already_running",
      "control operation already queued/running; no run, park, or tracker state was changed. Next safe command: scripts/scherzoctl query operation-status op-1 --json",
    ),
  ]

  list.each(cases, fn(entry) {
    let #(reason, message) = entry
    assert reason != ""
    assert string.contains(message, "Next safe command: scripts/scherzoctl ")
  })
}

pub fn retry_step_rejects_active_issue_for_interrupted_run_test() {
  let dir = "test/tmp/daemon-retry-step-active"
  let issue = issue("issue-1", "LIV-509", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_with_candidate(issue),
      hub_subject,
      fn(issue, _context, _effective) {
        process.send(log_subject, "agent_run:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "agent_run:issue-1", 100)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )

  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status) == Some("no_failed_workflow_run")

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_rejects_parked_issue_before_planning_test() {
  let dir = "test/tmp/daemon-retry-step-parked"
  let issue = issue("issue-1", "LIV-509", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: True)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let before = ledger_bodies(root)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )

  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status) == Some("issue_parked")
  let assert Some(message) = result.message
  assert string.contains(message, "operator_hold")
  assert string.contains(message, "scherzoctl unpark 'LIV-509'")
  assert ledger_bodies(root) == before

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_preserves_max_sessions_safety_park_test() {
  let dir = "test/tmp/daemon-retry-step-max-sessions-parked"
  let issue = issue("issue-1", "LIV-509", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  append_auto_unpark_issue_change_parked_record_with_reason(
    root,
    issue,
    "max_sessions_per_issue",
    10,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let identity = orchestrator_state.issue_identity(issue)
  let assert Ok(parked_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(parked_snapshot.parked, identity)
  let before = ledger_bodies(root)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )

  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status) == Some("issue_parked")
  let assert Some(message) = result.message
  assert string.contains(message, "max_sessions_per_issue")
  assert string.contains(message, "scherzoctl unpark 'LIV-509'")
  let assert Ok(still_parked_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(still_parked_snapshot.parked, identity)
  assert ledger_bodies(root) == before

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_clears_failure_quarantine_before_planning_test() {
  let dir = "test/tmp/daemon-retry-step-quarantine"
  let issue = issue("issue-1", "LIV-509", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  append_auto_unpark_issue_change_parked_record(root, issue, 10)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(issue, _, _) {
        process.send(log_subject, "recovered_worker_started:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let identity = orchestrator_state.issue_identity(issue)
  let assert Ok(parked_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(parked_snapshot.parked, identity)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )

  assert command.status_to_string(result.status) == "queued"
  let assert Ok(unparked_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert !dict.has_key(unparked_snapshot.parked, identity)
  assert ledger_has_issue_unparked(root, issue.id, "retry_step")

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_rejects_manual_drift_like_parked_issue_test() {
  let dir = "test/tmp/daemon-retry-step-manual-drift-like-parked"
  let issue = issue("issue-1", "LIV-1371", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  append_explicit_parked_record(
    root,
    issue,
    "issue_state_drift:non_active_state",
    20,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let before = ledger_bodies(root)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )

  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status) == Some("issue_parked")
  assert ledger_bodies(root) == before

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_dry_run_reports_safe_point_and_preserved_discarded_steps_test() {
  let dir = "test/tmp/daemon-retry-dry-run-plan"
  let issue = issue("issue-1", "LIV-1374", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let log_subject = process.new_subject()
  let worker_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(_, _, _) {
        process.send(worker_subject, "unexpected_worker_started")
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let before = ledger_bodies(root)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStepDryRun(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )

  assert command.status_to_string(result.status) == "applied"
  let assert Some(message) = result.message
  assert string.contains(
    message,
    "chosen safe point=resume from apply_feedback",
  )
  assert string.contains(message, "preserved steps=seed")
  assert string.contains(message, "discarded steps=apply_feedback")
  assert ledger_bodies(root) == before
  test_async.assert_no_extra_message(worker_subject)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_queues_operation_and_records_lifecycle_before_spawning_recovered_worker_test() {
  let dir = "test/tmp/daemon-retry-step-accepted"
  let issue = issue("issue-1", "LIV-509", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(expected_bundle_dir) = path.absolute(dir <> "/workflows")
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(issue, context, effective) {
        process.send(log_subject, "recovered_worker_started:" <> issue.id)
        process.send(
          log_subject,
          "recovered_bundle_dir:" <> context.workflow_bundle_dir,
        )
        process.send(
          log_subject,
          recovery_append_state(log_subject, effective.workspace.root),
        )
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )

  assert command.status_reason(result.status) == None
  assert command.status_to_string(result.status) == "queued"
  let assert Some(operation_id) = result.operation_id
  assert string.starts_with(operation_id, "retry-step:run-1:apply_feedback:")
  assert result.message
    == Some("retry-step accepted; poll query operation-status for completion")
  let assert Ok(queued_operation) =
    projection.control_operation(load_projection_or_panic(root), operation_id)
  assert queued_operation.requested_step_id == Some("apply_feedback")
  assert queued_operation.status == "queued"
    || queued_operation.status == "running"
  assert count_kind(root, "control_operation_queued") == 1

  assert wait_for_log(log_subject, "recovered_worker_started:issue-1", 100)
  assert wait_for_log(
    log_subject,
    "recovered_bundle_dir:" <> expected_bundle_dir,
    100,
  )
  assert wait_for_log(log_subject, "retry_step_ledger_ready", 100)
  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.message
    == Some(
      "provenance_ok; retrying run run-1 step apply_feedback at attempt 2",
    )
  assert contains_kind_sequence(root, [
    "control_operation_queued",
    "control_operation_started",
    "workflow_repair_requested",
    "step_attempt_superseded",
    "workflow_run_started",
    "known_workspace",
    "issue_counter_updated",
    "control_operation_completed",
  ])

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_accepts_issue_description_drift_and_records_retry_snapshot_test() {
  let dir = "test/tmp/daemon-retry-step-issue-drift"
  let original_issue = issue("issue-1", "LIV-1370", "Todo")
  let changed_issue =
    tracker_issue.Issue(
      ..original_issue,
      description: Some("Updated description"),
    )
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, original_issue, include_parked: False)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(changed_issue),
      hub_subject,
      fn(issue, context, _) {
        process.send(
          log_subject,
          "recovered_worker_started:"
            <> option.unwrap(issue.description, "missing"),
        )
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )
  let operation_id = assert_retry_step_queued(result, Some("apply_feedback"))
  assert wait_for_log(
    log_subject,
    "recovered_worker_started:Updated description",
    100,
  )

  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.requested_step_id == Some("apply_feedback")
  let changed_fingerprint = tracker_issue.content_fingerprint(changed_issue)
  assert list.any(ledger_bodies(root), fn(body) {
    case body {
      record.WorkflowRunStartedWithTask(
        run_id: body_run_id,
        issue_fingerprint: fingerprint,
        ..,
      ) -> body_run_id == "run-1" && fingerprint == changed_fingerprint
      _ -> False
    }
  })
  assert count_kind(root, "issue_parked_v2") == 0

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_unparks_issue_content_drift_parked_run_and_records_current_snapshot_test() {
  let dir = "test/tmp/daemon-retry-step-parked-issue-content-drift"
  let drift_reason = "issue_content_drift:issue_fingerprint_changed"
  let original_issue = issue("issue-1", "LIV-1370", "Todo")
  let changed_issue =
    tracker_issue.Issue(
      ..original_issue,
      description: Some("Updated description"),
    )
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run_with_interruption_reason(
    root,
    original_issue,
    drift_reason,
  )
  append_explicit_parked_record(root, original_issue, drift_reason, 20)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(changed_issue),
      hub_subject,
      fn(issue, context, _) {
        process.send(
          log_subject,
          "recovered_worker_started:"
            <> option.unwrap(issue.description, "missing"),
        )
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )
  let operation_id = assert_retry_step_queued(result, Some("apply_feedback"))
  assert wait_for_log(
    log_subject,
    "recovered_worker_started:Updated description",
    100,
  )

  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.requested_step_id == Some("apply_feedback")
  let changed_fingerprint = tracker_issue.content_fingerprint(changed_issue)
  assert list.any(ledger_bodies(root), fn(body) {
    case body {
      record.WorkflowRunStartedWithTask(
        run_id: body_run_id,
        issue_fingerprint: fingerprint,
        ..,
      ) -> body_run_id == "run-1" && fingerprint == changed_fingerprint
      _ -> False
    }
  })
  assert contains_kind_sequence(root, [
    "issue_parked_v2",
    "control_operation_queued",
    "control_operation_started",
    "issue_unparked",
    "workflow_repair_requested",
  ])

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_retry_step_exact_resume_validation_failure_is_operation_noop_test() {
  let dir = "test/tmp/daemon-retry-step-resume-validation-noop"
  let issue = issue("issue-1", "LIV-1368", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  let initial_fingerprint = workflow_fingerprint_for_config(workflow_path)
  seed_interrupted_retry_step_run_with_workflow_fingerprint(
    root,
    issue,
    include_parked: False,
    workflow_fingerprint: initial_fingerprint,
  )
  let log_subject = process.new_subject()
  let fetch_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let fetch_gate = start_fetch_counter()
  let tracker_client =
    tracker_issue_only_blocking_retry_operation_fetch(
      issue,
      log_subject,
      fetch_barrier,
      fetch_gate,
    )
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_client,
      tracker_adapter_with_transition_logging(log_subject, tracker_client),
      hub_subject,
      fn(issue, _, _) {
        process.send(log_subject, "unexpected_worker_started:" <> issue.id)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  let assert Ok(first_result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStepExact(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )
  let first_operation_id =
    assert_retry_step_queued(first_result, Some("apply_feedback"))
  arm_fetch_gate(fetch_gate)
  assert wait_for_log(log_subject, "retry_step_operation_fetch_blocked", 100)

  let assert Ok(Nil) =
    simplifile.write(dir <> "/workflows/prompts/task.md", "Changed prompt")
  let assert Ok(reload_result) =
    daemon.apply_operator_command(started.data, command.ReloadWorkflow, 1000)
  assert command.status_to_string(reload_result.status) == "applied"
  test_async.release_barrier(fetch_barrier)

  let assert Ok(first_operation) =
    wait_for_operation_status(root, first_operation_id, "failed", 50)
  assert first_operation.reason == Some("workflow_drift")
  let assert Some(first_message) = first_operation.message
  assert string.contains(
    first_message,
    "Next safe command: scripts/scherzoctl run retry-step run-1 --step apply_feedback",
  )
  assert count_kind(root, "workflow_repair_requested") == 0
  assert count_kind(root, "step_attempt_superseded") == 0
  assert count_kind(root, "workflow_run_finished") == 0
  assert count_kind(root, "issue_parked_v2") == 0

  let assert Ok(second_result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStepExact(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )
  let second_operation_id =
    assert_retry_step_queued(second_result, Some("apply_feedback"))
  let assert Ok(second_operation) =
    wait_for_operation_status(root, second_operation_id, "failed", 50)
  assert second_operation.reason == Some("workflow_drift")
  assert count_kind(root, "workflow_repair_requested") == 0
  assert count_kind(root, "step_attempt_superseded") == 0
  assert count_kind(root, "issue_parked_v2") == 0
  let assert Ok(_) =
    workflow_repair.resolve_target_run(
      load_projection_or_panic(root),
      command.RetryWorkflowStepRunId("run-1"),
    )
  assert !wait_for_log(log_subject, "unexpected_worker_started:issue-1", 5)
  assert !wait_for_log(log_subject, "state_transition:Todo", 5)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_repairs_claim_handoff_interrupted_run_test() {
  let dir = "test/tmp/daemon-retry-step-claim-handoff"
  let issue = issue("issue-1", "LIV-749", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_claim_handoff_interrupted_retry_step_run(root, issue)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(issue, context, _effective) {
        process.send(log_subject, "recovered_worker_started:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )
  let operation_id = assert_retry_step_queued(result, Some("apply_feedback"))

  assert contains_kind_sequence(root, [
    "workflow_run_started",
    "known_workspace",
    "run_started",
    "issue_counter_updated",
  ])
  assert wait_for_log(log_subject, "recovered_worker_started:issue-1", 100)
  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.requested_step_id == Some("apply_feedback")
  assert count_kind(root, "control_operation_queued") == 1
  assert count_kind(root, "control_operation_started") == 1
  assert count_kind(root, "workflow_repair_requested") == 1
  assert count_kind(root, "step_attempt_superseded") == 1
  assert count_kind(root, "control_operation_completed") == 1

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_repairs_missing_provenance_after_finalization_accepts_test() {
  let dir = "test/tmp/daemon-retry-step-missing-provenance"
  let issue = issue("issue-1", "LIV-695", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run_missing_provenance(root, issue)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(issue, context, _effective) {
        process.send(log_subject, "recovered_worker_started:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )
  let operation_id = assert_retry_step_queued(result, Some("apply_feedback"))

  assert wait_for_log(log_subject, "recovered_worker_started:issue-1", 100)
  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.message
    == Some(
      "provenance_repaired; retrying run run-1 step apply_feedback at attempt 2",
    )
  assert count_kind(root, "control_operation_queued") == 1
  assert count_kind(root, "control_operation_started") == 1
  assert count_kind(root, "workflow_run_provenance_repaired") == 1
  assert count_kind(root, "workflow_repair_requested") == 1
  assert count_kind(root, "step_attempt_superseded") == 1
  assert count_kind(root, "control_operation_completed") == 1

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_retry_step_exact_artifact_recovery_failure_returns_detail_and_retains_diagnostic_test() {
  let dir = "test/tmp/daemon-retry-step-artifact-detail"
  let issue = issue("issue-1", "LIV-509", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let artifact_ref = artifact_store.artifact_ref("run-1", "seed", 1)
  let artifact_path = root <> "/.scherzo-state/artifacts/" <> artifact_ref
  let assert Ok(original_contents) = simplifile.read(artifact_path)
  let expected_sha256 = hash.sha256_hex(original_contents)
  let corrupt_contents = "corrupted retained artifact"
  let current_sha256 = hash.sha256_hex(corrupt_contents)
  let assert Ok(Nil) = simplifile.write(artifact_path, corrupt_contents)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStepExact(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )

  let operation_id = assert_retry_step_queued(result, Some("apply_feedback"))
  let detail =
    "artifact_recovery_failed: step_id=seed artifact_ref="
    <> artifact_ref
    <> " reason=sha_mismatch expected_sha256="
    <> expected_sha256
    <> " current_sha256="
    <> current_sha256
  let assert Ok(failed_operation) =
    wait_for_operation_status(root, operation_id, "failed", 20)
  assert failed_operation.reason == Some("artifact_recovery_failed")
  let assert Some(failed_message) = failed_operation.message
  assert string.contains(failed_message, detail)
  assert string.contains(
    failed_message,
    "Next safe command: scripts/scherzoctl run retry-step run-1 --step apply_feedback",
  )
  assert retained_workflow_diagnostic_reason(root, detail)
  assert !retained_workflow_interruption_reason(root, detail)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_retry_step_exact_does_not_append_provenance_repair_when_finalization_rejects_test() {
  let dir = "test/tmp/daemon-retry-step-missing-provenance-corrupt-artifact"
  let issue = issue("issue-1", "LIV-696", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run_missing_provenance(root, issue)
  let artifact_ref = artifact_store.artifact_ref("run-1", "seed", 1)
  let artifact_path = root <> "/.scherzo-state/artifacts/" <> artifact_ref
  let assert Ok(Nil) =
    simplifile.write(artifact_path, "corrupted retained artifact")
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStepExact(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )
  let operation_id = assert_retry_step_queued(result, Some("apply_feedback"))

  let assert Ok(failed_operation) =
    wait_for_operation_status(root, operation_id, "failed", 20)
  assert failed_operation.reason == Some("artifact_recovery_failed")
  assert !contains_kind(root, "workflow_run_provenance_repaired")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_accepts_non_active_non_terminal_issue_state_for_retained_run_test() {
  let dir = "test/tmp/daemon-retry-step-non-active"
  let issue = issue("issue-1", "LIV-510", "Triage")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_with_candidate(issue),
      hub_subject,
      fn(issue, context, _) {
        process.send(log_subject, "recovered_worker_started:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "candidates_fetched", 100)
  assert !wait_for_log(log_subject, "dispatch_started", 5)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )
  let operation_id = assert_retry_step_queued(result, Some("apply_feedback"))
  assert wait_for_log(log_subject, "recovered_worker_started:issue-1", 100)

  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.requested_step_id == Some("apply_feedback")
  assert contains_kind(root, "workflow_repair_requested")

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_unparks_issue_state_drift_parked_non_active_issue_test() {
  let dir = "test/tmp/daemon-retry-step-parked-non-active"
  let issue = issue("issue-1", "LIV-1370", "Triage")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  let drift_reason = "issue_state_drift:non_active_state"
  seed_interrupted_retry_step_run_with_interruption_reason(
    root,
    issue,
    drift_reason,
  )
  append_explicit_parked_record(root, issue, drift_reason, 20)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(issue, context, _) {
        process.send(log_subject, "recovered_worker_started:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )
  let operation_id = assert_retry_step_queued(result, Some("apply_feedback"))
  assert wait_for_log(log_subject, "recovered_worker_started:issue-1", 100)

  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.requested_step_id == Some("apply_feedback")
  assert contains_kind_sequence(root, [
    "issue_parked_v2",
    "control_operation_queued",
    "control_operation_started",
    "issue_unparked",
    "workflow_repair_requested",
  ])

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_abort_of_recovered_parent_cleans_review_children_and_exposes_orphan_metadata_test() {
  let dir = "test/tmp/daemon-retry-step-orphaned-review-children"
  let issue = issue("issue-2", "LIV-512", "Todo")
  let #(workflow_path, root) = write_retry_step_review_workflow(dir)
  seed_interrupted_review_retry_step_run(root, issue)
  let log_subject = process.new_subject()
  let code_review_barrier = test_async.new_barrier()
  let security_review_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(issue, context, _effective) {
        case context.step_id {
          "implement" -> Ok(worker_success(issue, context.workspace_path))
          "code_review" -> {
            process.send(log_subject, "review_started:code_review")
            test_async.block_until_released(code_review_barrier)
            Error(agent_types.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("released")),
              workspace_path: Some(context.workspace_path),
              tokens: session_tokens.zero_token_totals(),
              final_issue: None,
            ))
          }
          "security_review" -> {
            process.send(log_subject, "review_started:security_review")
            test_async.block_until_released(security_review_barrier)
            Error(agent_types.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("released")),
              workspace_path: Some(context.workspace_path),
              tokens: session_tokens.zero_token_totals(),
              final_issue: None,
            ))
          }
          _ ->
            Error(agent_types.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("unexpected step")),
              workspace_path: Some(context.workspace_path),
              tokens: session_tokens.zero_token_totals(),
              final_issue: None,
            ))
        }
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(retry_result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(command.RetryWorkflowStepRunId("run-1"), None),
      1000,
    )
  let operation_id = assert_retry_step_queued(retry_result, None)
  assert wait_for_all_logs(
    log_subject,
    [
      "review_started:code_review",
      "review_started:security_review",
    ],
    100,
  )

  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.requested_step_id == None

  let assert Ok(parent_session) =
    wait_for_parent_session(hub_subject, issue.identifier, 20)
  let assert Ok(abort_result) =
    daemon.apply_operator_command(
      started.data,
      command.AbortSession(parent_session.session_id),
      1000,
    )
  assert command.status_to_string(abort_result.status) == "applied"

  let assert Ok(code_review_session) =
    wait_for_step_session(hub_subject, "code_review", 20)
  let assert Ok(security_review_session) =
    wait_for_step_session(hub_subject, "security_review", 20)
  assert code_review_session.status
    == event.Exited(session_reason.OperatorAbort)
  assert security_review_session.status
    == event.Exited(session_reason.OperatorAbort)
  assert_child_orphan_recovery(code_review_session, "code_review")
  assert_child_orphan_recovery(security_review_session, "security_review")

  let ps_json =
    protocol.success_response(
      "ps-1",
      protocol.list_sessions_data(event.SessionList(
        [code_review_session, security_review_session],
        42,
      )),
    )
    |> protocol.response_to_string
  assert string.contains(ps_json, "\"workflow_run_id\":\"run-1\"")
  assert string.contains(ps_json, "\"workflow_step_id\":\"code_review\"")
  assert string.contains(ps_json, "\"workflow_attempt_index\":1")
  assert string.contains(ps_json, "\"parent_session_id\":\"run-1\"")
  assert string.contains(
    ps_json,
    "\"orphan_status\":\"orphaned_parent_stopped\"",
  )
  assert string.contains(ps_json, "\"issue_state\":\"Todo\"")
  assert string.contains(
    ps_json,
    "\"recommended_action\":\"cleanup_orphan_steps\"",
  )

  let session_json =
    protocol.success_response(
      "session-1",
      protocol.session_data(Some(code_review_session)),
    )
    |> protocol.response_to_string
  assert string.contains(session_json, "\"workflow_step_id\":\"code_review\"")
  assert string.contains(
    session_json,
    "\"orphan_status\":\"orphaned_parent_stopped\"",
  )

  assert has_step_interrupted_reason(
    root,
    "code_review",
    "orphaned_parent_stopped",
  )
  assert has_step_interrupted_reason(
    root,
    "security_review",
    "orphaned_parent_stopped",
  )
  assert count_step_interrupted_reason(
      root,
      "code_review",
      "orphaned_parent_stopped",
    )
    == 1
  assert count_step_interrupted_reason(
      root,
      "security_review",
      "orphaned_parent_stopped",
    )
    == 1

  let code_review_session_id = code_review_session.session_id
  let before_cleanup = ledger_bodies(root)
  let assert Ok(cleanup_dry_run) =
    daemon.apply_operator_command(
      started.data,
      command.CleanupOrphanSteps("run-1", True),
      1000,
    )
  assert command.status_to_string(cleanup_dry_run.status) == "applied"
  assert cleanup_dry_run.message
    == Some(
      "dry run for run-1 parent=finished:cancelled candidates=none records=none",
    )
  assert ledger_bodies(root) == before_cleanup

  let assert Ok(cleanup_yes) =
    daemon.apply_operator_command(
      started.data,
      command.CleanupOrphanSteps("run-1", False),
      1000,
    )
  assert command.status_to_string(cleanup_yes.status) == "applied"
  assert cleanup_yes.message
    == Some(
      "cleaned orphaned YAML child steps for run-1 parent=finished:cancelled candidates=none records=none",
    )
  assert ledger_bodies(root) == before_cleanup

  let assert Ok(prompt_result) =
    daemon.apply_operator_command(
      started.data,
      command.PromptSession(code_review_session_id, "continue"),
      1000,
    )
  assert command.status_to_string(prompt_result.status) == "not_found"
  assert command.status_reason(prompt_result.status) == None

  test_async.release_barrier_if_waiting(code_review_barrier)
  test_async.release_barrier_if_waiting(security_review_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_shutdown_interrupts_active_review_children_with_registry_metadata_test() {
  let dir = "test/tmp/daemon-retry-step-shutdown-review-children"
  let issue = issue("issue-6", "LIV-516", "Todo")
  let #(workflow_path, root) = write_retry_step_review_workflow(dir)
  seed_interrupted_review_retry_step_run(root, issue)
  let log_subject = process.new_subject()
  let code_review_barrier = test_async.new_barrier()
  let security_review_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(issue, context, _effective) {
        case context.step_id {
          "implement" -> Ok(worker_success(issue, context.workspace_path))
          "code_review" -> {
            process.send(log_subject, "shutdown_review_started:code_review")
            test_async.block_until_released(code_review_barrier)
            Error(agent_types.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("released")),
              workspace_path: Some(context.workspace_path),
              tokens: session_tokens.zero_token_totals(),
              final_issue: None,
            ))
          }
          "security_review" -> {
            process.send(log_subject, "shutdown_review_started:security_review")
            test_async.block_until_released(security_review_barrier)
            Error(agent_types.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("released")),
              workspace_path: Some(context.workspace_path),
              tokens: session_tokens.zero_token_totals(),
              final_issue: None,
            ))
          }
          _ ->
            Error(agent_types.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("unexpected step")),
              workspace_path: Some(context.workspace_path),
              tokens: session_tokens.zero_token_totals(),
              final_issue: None,
            ))
        }
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(retry_result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(command.RetryWorkflowStepRunId("run-1"), None),
      1000,
    )
  let operation_id = assert_retry_step_queued(retry_result, None)
  assert wait_for_all_logs(
    log_subject,
    [
      "shutdown_review_started:code_review",
      "shutdown_review_started:security_review",
    ],
    100,
  )

  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.requested_step_id == None

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  assert has_step_interrupted_attempt_reason(
    root,
    "code_review",
    1,
    "daemon_shutdown",
  )
  assert has_step_interrupted_attempt_reason(
    root,
    "security_review",
    1,
    "daemon_shutdown",
  )
  assert count_step_interrupted_reason(root, "code_review", "daemon_shutdown")
    == 1
  assert count_step_interrupted_reason(
      root,
      "security_review",
      "daemon_shutdown",
    )
    == 1

  test_async.release_barrier_if_waiting(code_review_barrier)
  test_async.release_barrier_if_waiting(security_review_barrier)
  hub.stop(hub_subject)
}

fn wait_until_startup_recovery_ready(
  daemon_subject: process.Subject(daemon.Message),
) -> Nil {
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(daemon_subject, 1000)
  Nil
}

pub fn cleanup_orphan_steps_rejects_active_or_unknown_runs_and_reports_exact_records_test() {
  let active_dir = "test/tmp/daemon-cleanup-orphans-active"
  let active_issue = issue("issue-3", "LIV-513", "Todo")
  let #(active_workflow_path, active_root) =
    write_retry_step_review_workflow(active_dir)
  seed_interrupted_review_retry_step_run(active_root, active_issue)
  let log_subject = process.new_subject()
  let code_review_barrier = test_async.new_barrier()
  let security_review_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(active_issue),
      hub_subject,
      fn(issue, context, _effective) {
        case context.step_id {
          "implement" -> Ok(worker_success(issue, context.workspace_path))
          "code_review" -> {
            process.send(log_subject, "active_review_started:code_review")
            test_async.block_until_released(code_review_barrier)
            Error(agent_types.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("released")),
              workspace_path: Some(context.workspace_path),
              tokens: session_tokens.zero_token_totals(),
              final_issue: None,
            ))
          }
          "security_review" -> {
            process.send(log_subject, "active_review_started:security_review")
            test_async.block_until_released(security_review_barrier)
            Error(agent_types.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("released")),
              workspace_path: Some(context.workspace_path),
              tokens: session_tokens.zero_token_totals(),
              final_issue: None,
            ))
          }
          _ ->
            Error(agent_types.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("unexpected step")),
              workspace_path: Some(context.workspace_path),
              tokens: session_tokens.zero_token_totals(),
              final_issue: None,
            ))
        }
      },
    )
  let assert Ok(active_started) = daemon.start(Some(active_workflow_path), deps)
  wait_until_startup_recovery_ready(active_started.data)

  let assert Ok(retry_result) =
    daemon.apply_operator_command(
      active_started.data,
      command.RetryWorkflowStep(command.RetryWorkflowStepRunId("run-1"), None),
      1000,
    )
  let operation_id = assert_retry_step_queued(retry_result, None)
  assert wait_for_all_logs(
    log_subject,
    [
      "active_review_started:code_review",
      "active_review_started:security_review",
    ],
    100,
  )
  let assert Ok(completed_operation) =
    wait_for_operation_status(active_root, operation_id, "completed", 20)
  assert completed_operation.requested_step_id == None
  let assert Ok(active_code_review_session) =
    wait_for_active_step_session(hub_subject, "run-1", "code_review", 1, 20)
  let assert Ok(active_security_review_session) =
    wait_for_active_step_session(hub_subject, "run-1", "security_review", 1, 20)
  assert_active_child_has_no_orphan_cleanup_recovery(active_code_review_session)
  assert_active_child_has_no_orphan_cleanup_recovery(
    active_security_review_session,
  )

  let assert Ok(active_parent_result) =
    daemon.apply_operator_command(
      active_started.data,
      command.CleanupOrphanSteps("run-1", True),
      1000,
    )
  assert command.status_to_string(active_parent_result.status) == "rejected"
  assert command.status_reason(active_parent_result.status)
    == Some("parent_run_active")

  let assert Ok(unknown_run_result) =
    daemon.apply_operator_command(
      active_started.data,
      command.CleanupOrphanSteps("run-missing", True),
      1000,
    )
  assert command.status_to_string(unknown_run_result.status) == "not_found"
  assert unknown_run_result.message
    == Some("workflow run not found: run-missing")

  test_async.release_barrier_if_waiting(code_review_barrier)
  test_async.release_barrier_if_waiting(security_review_barrier)
  assert daemon.shutdown(active_started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)

  let retained_dir = "test/tmp/daemon-cleanup-orphans-retained"
  let retained_issue = issue("issue-4", "LIV-514", "Todo")
  let #(retained_workflow_path, retained_root) =
    write_retry_step_review_workflow(retained_dir)
  seed_orphaned_review_children_run(retained_root, retained_issue)
  let retained_log_subject = process.new_subject()
  let assert Ok(retained_hub_subject) = hub.start(50, fn() { 42 })
  let retained_deps =
    in_process_dependencies(
      retained_log_subject,
      tracker_issue_only(retained_issue),
      retained_hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(retained_started) =
    daemon.start(Some(retained_workflow_path), retained_deps)
  wait_until_startup_recovery_ready(retained_started.data)
  let before_dry_run = ledger_bodies(retained_root)

  let assert Ok(dry_run_result) =
    daemon.apply_operator_command(
      retained_started.data,
      command.CleanupOrphanSteps("run-1", True),
      1000,
    )
  assert command.status_to_string(dry_run_result.status) == "applied"
  let assert Some(dry_run_message) = dry_run_result.message
  assert string.contains(dry_run_message, "dry run for run-1")
  assert string.contains(dry_run_message, "parent=interrupted:daemon_shutdown")
  assert string.contains(
    dry_run_message,
    "code_review#1 workflow=implementation",
  )
  assert string.contains(
    dry_run_message,
    "security_review#1 workflow=implementation",
  )
  assert string.contains(
    dry_run_message,
    "step_attempt_interrupted(run_id=run-1, workflow_id=implementation, step_id=code_review, attempt_index=1, reason=orphaned_parent_stopped)",
  )
  assert string.contains(
    dry_run_message,
    "step_attempt_interrupted(run_id=run-1, workflow_id=implementation, step_id=security_review, attempt_index=1, reason=orphaned_parent_stopped)",
  )
  assert ledger_bodies(retained_root) == before_dry_run

  let assert Ok(cleanup_result) =
    daemon.apply_operator_command(
      retained_started.data,
      command.CleanupOrphanSteps("run-1", False),
      1000,
    )
  assert command.status_to_string(cleanup_result.status) == "applied"
  assert has_step_interrupted_reason(
    retained_root,
    "code_review",
    "orphaned_parent_stopped",
  )
  assert has_step_interrupted_reason(
    retained_root,
    "security_review",
    "orphaned_parent_stopped",
  )
  assert count_step_interrupted_reason(
      retained_root,
      "code_review",
      "orphaned_parent_stopped",
    )
    == 1
  assert count_step_interrupted_reason(
      retained_root,
      "security_review",
      "orphaned_parent_stopped",
    )
    == 1

  let after_first_cleanup = ledger_bodies(retained_root)
  let assert Ok(repeat_cleanup_result) =
    daemon.apply_operator_command(
      retained_started.data,
      command.CleanupOrphanSteps("run-1", False),
      1000,
    )
  assert command.status_to_string(repeat_cleanup_result.status) == "applied"
  assert repeat_cleanup_result.message
    == Some(
      "cleaned orphaned YAML child steps for run-1 parent=interrupted:daemon_shutdown candidates=none records=none",
    )
  assert ledger_bodies(retained_root) == after_first_cleanup

  assert daemon.shutdown(retained_started.data, 1000) == Ok(Nil)
  hub.stop(retained_hub_subject)
}

pub fn retry_step_active_command_session_has_no_orphan_cleanup_recovery_test() {
  let dir = "test/tmp/daemon-retry-step-active-command"
  let active_issue = issue("issue-5", "LIV-515", "Todo")
  let #(active_workflow_path, active_root) =
    write_retry_command_step_workflow(dir)
  seed_interrupted_retry_step_run(
    active_root,
    active_issue,
    include_parked: False,
  )
  let log_subject = process.new_subject()
  let command_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let base_deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(active_issue),
      hub_subject,
      fn(_, context, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError(
            "unexpected agent step: " <> context.step_id,
          )),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let deps =
    daemon.RuntimeDependencies(
      ..base_deps,
      workflow_run_dependencies: workflow_run.Dependencies(
        ..base_deps.workflow_run_dependencies,
        command_step: fn(
          context: workflow_run.StepContext,
          _command: String,
          _timeout_ms: Int,
          _secrets: List(String),
          limits: config_types.ArtifactLimits,
        ) {
          process.send(
            log_subject,
            "active_command_started:" <> context.step_id,
          )
          test_async.block_until_released(command_barrier)
          step_artifact.from_command_result(
            context.step_id,
            0,
            "done",
            "",
            False,
            [],
            limits,
          )
        },
      ),
    )
  let assert Ok(active_started) = daemon.start(Some(active_workflow_path), deps)
  wait_until_startup_recovery_ready(active_started.data)

  let assert Ok(retry_result) =
    daemon.apply_operator_command(
      active_started.data,
      command.RetryWorkflowStep(command.RetryWorkflowStepRunId("run-1"), None),
      1000,
    )
  let operation_id = assert_retry_step_queued(retry_result, None)
  assert wait_for_log(log_subject, "active_command_started:apply_feedback", 100)

  let assert Ok(completed_operation) =
    wait_for_operation_status(active_root, operation_id, "completed", 20)
  assert completed_operation.requested_step_id == None

  let assert Ok(active_command_session) =
    wait_for_active_step_session(hub_subject, "run-1", "apply_feedback", 2, 100)
  assert_active_child_has_no_orphan_cleanup_recovery(active_command_session)

  let assert Ok(active_parent_result) =
    daemon.apply_operator_command(
      active_started.data,
      command.CleanupOrphanSteps("run-1", True),
      1000,
    )
  assert command.status_to_string(active_parent_result.status) == "rejected"
  assert command.status_reason(active_parent_result.status)
    == Some("parent_run_active")

  test_async.release_barrier_if_waiting(command_barrier)
  assert daemon.shutdown(active_started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_non_active_parent_stop_interrupts_command_child_test() {
  let dir = "test/tmp/daemon-retry-step-command-non-active-stop"
  let active_issue = issue("issue-6", "LIV-932", "Todo")
  let non_active_issue = issue("issue-6", "LIV-932", "Triage")
  let #(workflow_path, root) = write_retry_command_step_workflow(dir)
  seed_interrupted_retry_step_run(root, active_issue, include_parked: False)
  let log_subject = process.new_subject()
  let issue_subject = start_issue_sequence(active_issue)
  let command_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let base_deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_sequence(issue_subject),
      hub_subject,
      fn(_, context, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError(
            "unexpected agent step: " <> context.step_id,
          )),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let deps =
    daemon.RuntimeDependencies(
      ..base_deps,
      workflow_run_dependencies: workflow_run.Dependencies(
        ..base_deps.workflow_run_dependencies,
        command_step: fn(
          context: workflow_run.StepContext,
          _command: String,
          _timeout_ms: Int,
          _secrets: List(String),
          limits: config_types.ArtifactLimits,
        ) {
          process.send(
            log_subject,
            "active_command_started:" <> context.step_id,
          )
          test_async.block_until_released(command_barrier)
          step_artifact.from_command_result(
            context.step_id,
            0,
            "done",
            "",
            False,
            [],
            limits,
          )
        },
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(retry_result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(command.RetryWorkflowStepRunId("run-1"), None),
      1000,
    )
  let operation_id = assert_retry_step_queued(retry_result, None)
  assert wait_for_log(log_subject, "active_command_started:apply_feedback", 100)

  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.requested_step_id == None

  let assert Ok(active_command_session) =
    wait_for_active_step_session(hub_subject, "run-1", "apply_feedback", 2, 100)
  assert_active_child_has_no_orphan_cleanup_recovery(active_command_session)

  set_issue_sequence(issue_subject, non_active_issue)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "worker_stop_requested", 100)

  let assert Ok(stopped_command_session) =
    wait_for_step_session(hub_subject, "apply_feedback", 20)
  assert stopped_command_session.status == event.Exited(session_reason.Stopped)
  assert_child_orphan_recovery_for_attempt(
    stopped_command_session,
    "apply_feedback",
    2,
    "Todo",
  )
  assert has_step_interrupted_attempt_reason(
    root,
    "apply_feedback",
    2,
    "orphaned_parent_stopped",
  )
  assert !has_step_finished_attempt(root, "apply_feedback", 2)

  test_async.release_barrier_if_waiting(command_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(issue_subject, IssueSequenceStop)
  hub.stop(hub_subject)
}

pub fn retry_step_rejects_terminal_issue_state_for_retained_run_test() {
  let dir = "test/tmp/daemon-retry-step-terminal"
  let issue = issue("issue-1", "LIV-511", "Done")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let before = ledger_bodies(root)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )

  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status)
    == Some("issue_state_drift:terminal_state")
  let assert Some(message) = result.message
  assert string.contains(
    message,
    "Next safe command: scripts/scherzoctl task show LIV-511 --json",
  )
  assert ledger_bodies(root) == before

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_startup_replay_replays_queued_operation_test() {
  let dir = "test/tmp/daemon-retry-step-startup-replay-queued"
  let issue = issue("issue-1", "LIV-1262", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let operation_id = "retry-step:run-1:apply_feedback:queued-replay"
  append_ledger_records(root, [
    record.with_id(
      "queued-op",
      40,
      record.ControlOperationQueued(
        operation_id: operation_id,
        operation_kind: "retry_step",
        command_name: "retry_step",
        target: "run-1",
        run_id: Some("run-1"),
        issue_id: Some(issue.id),
        issue_identifier: Some(issue.identifier),
        requested_step_id: Some("apply_feedback"),
        publication_id: None,
      ),
    ),
  ])
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(issue, context, _effective) {
        process.send(log_subject, "startup_replay_worker_started:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  assert wait_for_log(log_subject, "startup_replay_worker_started:issue-1", 100)
  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.requested_step_id == Some("apply_feedback")
  assert count_kind(root, "workflow_repair_requested") == 1

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_startup_replay_clears_failure_quarantine_test() {
  let dir = "test/tmp/daemon-retry-step-startup-replay-quarantine"
  let issue = issue("issue-1", "LIV-1262", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  append_auto_unpark_issue_change_parked_record(root, issue, 10)
  let operation_id = "retry-step:run-1:apply_feedback:queued-quarantine"
  append_ledger_records(root, [
    record.with_id(
      "queued-op",
      40,
      record.ControlOperationQueued(
        operation_id: operation_id,
        operation_kind: "retry_step",
        command_name: "retry_step",
        target: "run-1",
        run_id: Some("run-1"),
        issue_id: Some(issue.id),
        issue_identifier: Some(issue.identifier),
        requested_step_id: Some("apply_feedback"),
        publication_id: None,
      ),
    ),
  ])
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(issue, context, _effective) {
        process.send(log_subject, "startup_replay_worker_started:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  assert wait_for_log(log_subject, "startup_replay_worker_started:issue-1", 100)
  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.requested_step_id == Some("apply_feedback")
  assert ledger_has_issue_unparked(root, issue.id, "retry_step")
  assert ledger_has_issue_counter_reset(root, issue.id)
  assert contains_kind_sequence(root, [
    "issue_unparked",
    "issue_counter_updated",
    "workflow_repair_requested",
  ])
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert !dict.has_key(
    snapshot.parked,
    orchestrator_state.issue_identity(issue),
  )

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_operation_status_query_succeeds_while_startup_replay_is_running_test() {
  let dir = "test/tmp/daemon-retry-step-operation-status-running"
  let issue = issue("issue-1", "LIV-1262", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let operation_id = "retry-step:run-1:apply_feedback:running-query"
  append_ledger_records(root, [
    record.with_id(
      "queued-op",
      40,
      record.ControlOperationQueued(
        operation_id: operation_id,
        operation_kind: "retry_step",
        command_name: "retry_step",
        target: "run-1",
        run_id: Some("run-1"),
        issue_id: Some(issue.id),
        issue_identifier: Some(issue.identifier),
        requested_step_id: Some("apply_feedback"),
        publication_id: None,
      ),
    ),
  ])
  let log_subject = process.new_subject()
  let lookup_barrier = test_async.new_barrier()
  let tracker_client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        process.send(log_subject, "startup_replay_issue_lookup")
        test_async.block_until_released(lookup_barrier)
        Ok([issue])
      },
    )
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  assert wait_for_log(log_subject, "startup_replay_issue_lookup", 100)
  let assert Ok(running_operation) =
    wait_for_operation_status(root, operation_id, "running", 20)
  assert running_operation.requested_step_id == Some("apply_feedback")

  let assert Ok(query_types.OperationStatusResponse(operation)) =
    daemon.execute_query(
      started.data,
      query_types.OperationStatus(query_types.OperationStatusQuery(
        operation_id: operation_id,
      )),
      1000,
    )
  assert operation.operation_id == operation_id
  assert operation.status == "running"

  test_async.release_barrier_if_waiting(lookup_barrier)
  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.requested_step_id == Some("apply_feedback")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_startup_replay_replays_running_operation_without_duplicate_started_record_test() {
  let dir = "test/tmp/daemon-retry-step-startup-replay-running"
  let issue = issue("issue-1", "LIV-1262", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let operation_id = "retry-step:run-1:apply_feedback:running-replay"
  append_ledger_records(root, [
    record.with_id(
      "queued-op",
      40,
      record.ControlOperationQueued(
        operation_id: operation_id,
        operation_kind: "retry_step",
        command_name: "retry_step",
        target: "run-1",
        run_id: Some("run-1"),
        issue_id: Some(issue.id),
        issue_identifier: Some(issue.identifier),
        requested_step_id: Some("apply_feedback"),
        publication_id: None,
      ),
    ),
    record.with_id(
      "started-op",
      41,
      record.ControlOperationStarted(operation_id),
    ),
  ])
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(issue, context, _effective) {
        process.send(log_subject, "startup_running_worker_started:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  assert wait_for_log(
    log_subject,
    "startup_running_worker_started:issue-1",
    100,
  )
  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.started_at_ms == Some(41)
  assert count_kind(root, "control_operation_started") == 1
  assert count_kind(root, "workflow_repair_requested") == 1

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_startup_replay_skips_completed_and_failed_operations_test() {
  let dir = "test/tmp/daemon-retry-step-startup-replay-skip-terminal"
  let issue = issue("issue-1", "LIV-1262", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  append_ledger_records(root, [
    record.with_id(
      "completed-queued",
      40,
      record.ControlOperationQueued(
        operation_id: "retry-step:run-1:apply_feedback:completed",
        operation_kind: "retry_step",
        command_name: "retry_step",
        target: "run-1",
        run_id: Some("run-1"),
        issue_id: Some(issue.id),
        issue_identifier: Some(issue.identifier),
        requested_step_id: Some("apply_feedback"),
        publication_id: None,
      ),
    ),
    record.with_id(
      "completed-final",
      41,
      record.ControlOperationCompleted(
        operation_id: "retry-step:run-1:apply_feedback:completed",
        message: Some("done"),
      ),
    ),
    record.with_id(
      "failed-queued",
      42,
      record.ControlOperationQueued(
        operation_id: "retry-step:run-1:apply_feedback:failed",
        operation_kind: "retry_step",
        command_name: "retry_step",
        target: "run-1",
        run_id: Some("run-1"),
        issue_id: Some(issue.id),
        issue_identifier: Some(issue.identifier),
        requested_step_id: Some("apply_feedback"),
        publication_id: None,
      ),
    ),
    record.with_id(
      "failed-final",
      43,
      record.ControlOperationFailed(
        operation_id: "retry-step:run-1:apply_feedback:failed",
        reason: "artifact_recovery_failed",
        message: Some("failed"),
      ),
    ),
  ])
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_issue_only(issue),
      hub_subject,
      fn(issue, _context, _effective) {
        process.send(log_subject, "unexpected_start:" <> issue.id)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  assert !wait_for_log(log_subject, "unexpected_start:issue-1", 5)
  assert count_kind(root, "workflow_repair_requested") == 0
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_replay_is_idempotent_and_queue_append_failure_rejects_without_async_work_test() {
  let duplicate_dir = "test/tmp/daemon-retry-step-replay-idempotent"
  let duplicate_issue = issue("issue-1", "LIV-1262", "Todo")
  let #(duplicate_workflow_path, duplicate_root) =
    write_retry_step_workflow(duplicate_dir)
  seed_interrupted_retry_step_run(
    duplicate_root,
    duplicate_issue,
    include_parked: False,
  )
  let duplicate_log_subject = process.new_subject()
  let duplicate_worker_barrier = test_async.new_barrier()
  let assert Ok(duplicate_hub_subject) = hub.start(50, fn() { 42 })
  let duplicate_deps =
    in_process_dependencies(
      duplicate_log_subject,
      tracker_issue_only(duplicate_issue),
      duplicate_hub_subject,
      fn(issue, context, _effective) {
        process.send(
          duplicate_log_subject,
          "duplicate_worker_started:" <> issue.id,
        )
        test_async.block_until_released(duplicate_worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: Some(context.workspace_path),
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(duplicate_started) =
    daemon.start(Some(duplicate_workflow_path), duplicate_deps)
  wait_until_startup_recovery_ready(duplicate_started.data)

  let assert Ok(duplicate_result) =
    daemon.apply_operator_command(
      duplicate_started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )
  let duplicate_operation_id =
    assert_retry_step_queued(duplicate_result, Some("apply_feedback"))
  process.send(
    duplicate_started.data,
    daemon.RunQueuedControlOperation(duplicate_operation_id),
  )
  process.send(
    duplicate_started.data,
    daemon.RunQueuedControlOperation(duplicate_operation_id),
  )

  assert wait_for_log(
    duplicate_log_subject,
    "duplicate_worker_started:issue-1",
    100,
  )
  let assert Ok(duplicate_completed) =
    wait_for_operation_status(
      duplicate_root,
      duplicate_operation_id,
      "completed",
      20,
    )
  assert duplicate_completed.requested_step_id == Some("apply_feedback")
  process.send(
    duplicate_started.data,
    daemon.RunQueuedControlOperation(duplicate_operation_id),
  )
  assert count_kind(duplicate_root, "workflow_repair_requested") == 1
  assert count_kind(duplicate_root, "step_attempt_superseded") == 1

  test_async.release_barrier_if_waiting(duplicate_worker_barrier)
  assert daemon.shutdown(duplicate_started.data, 1000) == Ok(Nil)
  hub.stop(duplicate_hub_subject)

  let failure_dir = "test/tmp/daemon-retry-step-queue-append-failed"
  let failure_issue = issue("issue-1", "LIV-1262", "Todo")
  let #(failure_workflow_path, failure_root) =
    write_retry_step_workflow(failure_dir)
  seed_interrupted_retry_step_run(
    failure_root,
    failure_issue,
    include_parked: False,
  )
  let failure_log_subject = process.new_subject()
  let assert Ok(failure_hub_subject) = hub.start(50, fn() { 42 })
  let failure_deps =
    in_process_dependencies(
      failure_log_subject,
      tracker_issue_only(failure_issue),
      failure_hub_subject,
      fn(issue, _context, _effective) {
        process.send(
          failure_log_subject,
          "queue_append_failed_spawn:" <> issue.id,
        )
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(failure_started) =
    daemon.start(Some(failure_workflow_path), failure_deps)
  wait_until_startup_recovery_ready(failure_started.data)
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(failure_root)
  chmod_path("a-w", ledger_path.current_path)

  let assert Ok(failure_result) =
    daemon.apply_operator_command(
      failure_started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )

  chmod_path("u+w", ledger_path.current_path)
  assert command.status_to_string(failure_result.status) == "rejected"
  assert command.status_reason(failure_result.status)
    == Some("ledger_append_failed")
  assert failure_result.message == Some("failed to append retry-step operation")
  assert count_kind(failure_root, "control_operation_queued") == 0
  assert count_kind(failure_root, "workflow_repair_requested") == 0
  assert !wait_for_log(
    failure_log_subject,
    "queue_append_failed_spawn:issue-1",
    5,
  )

  assert daemon.shutdown(failure_started.data, 1000) == Ok(Nil)
  hub.stop(failure_hub_subject)
}

fn write_retry_step_workflow(dir: String) -> #(String, String) {
  test_helpers.reset_dir(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) = simplifile.write(prompt_dir <> "/task.md", "Prompt")
  let assert Ok(Nil) = simplifile.write(config_path, "version: 1
tracker:
  linear:
    api_key_env: HOME
    project: TEST
  states:
    ready: [Todo]
    active: [Todo]
    terminal: [Done]
workspace:
  root: " <> root <> "
agents:
  concurrency: 1
  sessions_per_task: 3
  runtime:
    type: pi
    pi:
      executable: fake
task_routing:
  labels:
    require_exactly_one: false
    default_workflow: implementation
workflows:
  implementation: workflows/implementation.yaml
")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: seed
    kind: command
    run: seed
    run_in: seed
  - id: apply_feedback
    kind: agent
    prompt: prompts/task.md
    depends_on: [seed]
    run_in:
      name: derived
      from: seed
",
    )
  #(config_path, root)
}

fn workflow_fingerprint_for_config(config_path: String) -> String {
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, workflow)) =
    runtime_bundle.workflow_by_id(bundle, "implementation")
  let assert Ok(fingerprint) =
    workflow_fingerprint_module.fingerprint_for_execution(
      workflow,
      bundle.orchestrator,
    )
  fingerprint
}

fn write_retry_command_step_workflow(dir: String) -> #(String, String) {
  test_helpers.reset_dir(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(Nil) = simplifile.write(config_path, "version: 1
tracker:
  linear:
    api_key_env: HOME
    project: TEST
  states:
    ready: [Todo]
    active: [Todo]
    terminal: [Done]
workspace:
  root: " <> root <> "
agents:
  concurrency: 1
  sessions_per_task: 3
  runtime:
    type: pi
    pi:
      executable: fake
task_routing:
  labels:
    require_exactly_one: false
    default_workflow: implementation
workflows:
  implementation: workflows/implementation.yaml
")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: seed
    kind: command
    run: seed
    run_in: seed
  - id: apply_feedback
    kind: command
    run: apply_feedback
    depends_on: [seed]
    run_in:
      name: derived
      from: seed
",
    )
  #(config_path, root)
}

fn write_retry_step_review_workflow(dir: String) -> #(String, String) {
  test_helpers.reset_dir(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) = simplifile.write(prompt_dir <> "/task.md", "Prompt")
  let assert Ok(Nil) = simplifile.write(config_path, "version: 1
tracker:
  linear:
    api_key_env: HOME
    project: TEST
  states:
    ready: [Todo]
    active: [Todo]
    terminal: [Done]
workspace:
  root: " <> root <> "
agents:
  concurrency: 1
  sessions_per_task: 3
  runtime:
    type: pi
    pi:
      executable: fake
task_routing:
  labels:
    require_exactly_one: false
    default_workflow: implementation
workflows:
  implementation: workflows/implementation.yaml
")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
concurrency: 2
steps:
  - id: seed
    kind: command
    run: seed
    run_in: seed
  - id: implement
    kind: agent
    prompt: prompts/task.md
    depends_on: [seed]
    run_in:
      name: main
      from: seed
  - id: code_review
    kind: agent
    prompt: prompts/task.md
    depends_on: [implement]
    run_in:
      name: code-review
      from: main
  - id: security_review
    kind: agent
    prompt: prompts/task.md
    depends_on: [implement]
    run_in:
      name: security-review
      from: main
  - id: finalize
    kind: command
    run: finalize
    depends_on: [code_review, security_review]
    run_in: main
",
    )
  #(config_path, root)
}

pub fn dispatch_recovery_classifier_marks_fresh_issue_without_retained_run_test() {
  let issue = issue("issue-1", "LIV-1059", "Todo")
  let #(workflow_path, _) =
    write_retry_step_workflow("test/tmp/dispatch-recovery-classifier-fresh")
  let projected = projection.fold([])

  assert dispatch_recovery.classify(
      projected,
      issue,
      observation_for(workflow_path, issue),
    )
    == dispatch_recovery.FreshDispatch
}

pub fn dispatch_recovery_classifier_uses_retry_step_for_interrupted_run_test() {
  let issue = issue("issue-1", "LIV-509", "Todo")
  let #(workflow_path, root) =
    write_retry_step_workflow("test/tmp/dispatch-recovery-classifier-step")
  seed_interrupted_retry_step_run(root, issue, include_parked: False)

  let assert dispatch_recovery.StepRecovery(_) =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      issue,
      observation_for(workflow_path, issue),
    )
}

pub fn dispatch_recovery_classifier_bypasses_retained_run_after_auto_unpark_issue_change_test() {
  let issue = issue("issue-1", "LIV-509", "Todo")
  let changed_issue = tracker_issue.Issue(..issue, title: "Changed title")
  let #(workflow_path, root) =
    write_retry_step_workflow(
      "test/tmp/dispatch-recovery-classifier-auto-unpark-changed",
    )
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "issue-auto-parked",
          20,
          record.IssueParkedV2(
            issue.id,
            issue.identifier,
            "dispatch_recovery_rejected",
            "auto_unpark_on_issue_change",
            tracker_issue.content_fingerprint(issue),
            20,
          ),
        ),
      ],
      True,
    )

  assert dispatch_recovery.classify(
      load_projection_or_panic(root),
      changed_issue,
      observation_for(workflow_path, changed_issue),
    )
    == dispatch_recovery.FreshDispatch
}

pub fn dispatch_recovery_classifier_rejects_missing_publication_manifest_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-publication-missing"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: False,
  )

  let assert dispatch_recovery.RejectRecovery(reason, _) =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      issue,
      observation_for(workflow_path, issue),
    )
  assert reason == "publication_retry_output_manifest_missing"
}

pub fn dispatch_recovery_classifier_uses_fresh_dispatch_when_retained_run_has_no_publications_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-publication-none"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_finished_publication_run_without_attempts(root, issue, "run-1", 1000)

  assert dispatch_recovery.classify(
      load_projection_or_panic(root),
      issue,
      observation_for(workflow_path, issue),
    )
    == dispatch_recovery.FreshDispatch
}

pub fn dispatch_recovery_classifier_supersedes_publication_issue_drift_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-publication-drift"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let changed_issue = tracker_issue.Issue(..issue, title: "Changed title")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )

  let assert dispatch_recovery.FreshSupersedingDispatch(
    "run-1",
    "execplan",
    reason,
    _,
  ) =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      changed_issue,
      observation_for(workflow_path, changed_issue),
    )
  assert reason == "publication_recovery_issue_drift"
}

pub fn dispatch_recovery_classifier_supersedes_publication_workflow_drift_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-publication-workflow-drift"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  drift_retry_publication_workflow(workflow_path)

  let assert dispatch_recovery.FreshSupersedingDispatch(
    "run-1",
    "execplan",
    reason,
    _,
  ) =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      issue,
      observation_for(workflow_path, issue),
    )
  assert reason == "publication_recovery_workflow_drift"
}

pub fn dispatch_recovery_classifier_requeues_publication_observation_failures_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-publication-requeue"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(_, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let projected = load_projection_or_panic(root)
  [
    #(recovery.IssueUnavailable, "publication_recovery_issue_unavailable"),
    #(
      recovery.TrackerRefreshUnavailable,
      "publication_recovery_tracker_refresh_unavailable",
    ),
    #(
      recovery.WorkflowUnavailable("workflow load failed"),
      "publication_recovery_workflow_unavailable",
    ),
  ]
  |> list.each(fn(entry) {
    let #(observation, expected_reason) = entry
    let assert dispatch_recovery.RequeueRecovery(reason, _) =
      dispatch_recovery.classify(projected, issue, observation)
    assert reason == expected_reason
  })
}

pub fn dispatch_recovery_classifier_supersedes_missing_publication_provenance_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-publication-provenance"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let projected =
    projection.Projection(
      ..load_projection_or_panic(root),
      workflow_run_provenances: dict.new(),
    )

  let assert dispatch_recovery.FreshSupersedingDispatch(
    "run-1",
    "execplan",
    reason,
    _,
  ) =
    dispatch_recovery.classify(
      projected,
      issue,
      observation_for(workflow_path, issue),
    )
  assert reason == "publication_recovery_provenance_missing"
}

pub fn dispatch_recovery_classifier_skips_retained_run_with_published_publication_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-publication-complete"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_recovered_published_publication_run(root, issue, "run-1", 1000)

  let assert dispatch_recovery.PublicationAlreadyPublished("run-1", "execplan") =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      issue,
      observation_for(workflow_path, issue),
    )
}

pub fn dispatch_recovery_classifier_supersedes_published_publication_issue_drift_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-published-issue-drift"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let changed_issue = tracker_issue.Issue(..issue, title: "Changed title")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_recovered_published_publication_run(root, issue, "run-1", 1000)

  let assert dispatch_recovery.FreshSupersedingDispatch(
    "run-1",
    "execplan",
    reason,
    _,
  ) =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      changed_issue,
      observation_for(workflow_path, changed_issue),
    )
  assert reason == "publication_recovery_issue_drift"
}

pub fn dispatch_recovery_classifier_supersedes_published_publication_workflow_drift_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-published-workflow-drift"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_recovered_published_publication_run(root, issue, "run-1", 1000)
  drift_retry_publication_workflow(workflow_path)

  let assert dispatch_recovery.FreshSupersedingDispatch(
    "run-1",
    "execplan",
    reason,
    _,
  ) =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      issue,
      observation_for(workflow_path, issue),
    )
  assert reason == "publication_recovery_workflow_drift"
}

pub fn dispatch_recovery_classifier_reports_existing_superseding_publication_run_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-publication-superseded"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  seed_existing_superseding_run(root, issue, "run-1", "run-2", 1030)

  let assert dispatch_recovery.SupersedingRunAlreadyExists("run-1", "run-2") =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      issue,
      observation_for(workflow_path, issue),
    )
}

pub fn dispatch_recovery_classifier_skips_retained_run_with_unchanged_publication_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-publication-unchanged"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_recovered_published_publication_run(root, issue, "run-1", 1000)
  seed_non_retryable_unchanged_publication_attempt(root, "run-1", at_ms: 1060)

  let assert dispatch_recovery.PublicationAlreadyPublished("run-1", "execplan") =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      issue,
      observation_for(workflow_path, issue),
    )
}

pub fn dispatch_recovery_classifier_skips_multi_publication_when_required_complete_and_optional_failed_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-publication-multi-complete"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_multi_publication_workflow(dir)
  seed_recovered_publication_attempts_run(root, issue, "run-1", 1000, [
    seed_publication_attempt("execplan_review_doc", "published", required: True),
    seed_publication_attempt(
      "execplan_supporting_doc",
      "published",
      required: True,
    ),
    seed_publication_attempt(
      "execplan_optional_note",
      "failed",
      required: False,
    ),
  ])

  let assert dispatch_recovery.PublicationAlreadyPublished("run-1", "execplan") =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      issue,
      observation_for(workflow_path, issue),
    )
}

pub fn dispatch_recovery_classifier_rejects_multi_publication_with_failed_required_publication_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-publication-multi-failed"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_multi_publication_workflow(dir)
  seed_recovered_publication_attempts_run(root, issue, "run-1", 1000, [
    seed_publication_attempt("execplan_review_doc", "published", required: True),
    seed_publication_attempt(
      "execplan_supporting_doc",
      "failed",
      required: True,
    ),
    seed_publication_attempt(
      "execplan_optional_note",
      "failed",
      required: False,
    ),
  ])

  let assert dispatch_recovery.RejectRecovery(reason, _) =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      issue,
      observation_for(workflow_path, issue),
    )
  assert reason == "publication_retry_targets_not_found"
}

pub fn dispatch_recovery_classifier_rejects_non_retryable_failed_publication_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-publication-nonretryable"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  seed_non_retryable_failed_publication_attempt(root, "run-1", at_ms: 1030)

  let assert dispatch_recovery.RejectRecovery(reason, _) =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      issue,
      observation_for(workflow_path, issue),
    )
  assert reason == "publication_retry_targets_not_found"
}

pub fn dispatch_recovery_classifier_rejects_newer_unsafe_publication_run_test() {
  let dir = "test/tmp/dispatch-recovery-classifier-publication-latest"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-2",
    2000,
    include_output_manifest: False,
  )

  let assert dispatch_recovery.RejectRecovery(reason, _) =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      issue,
      observation_for(workflow_path, issue),
    )
  assert reason == "publication_retry_output_manifest_missing"
}

pub fn dispatch_recovery_rejects_unsafe_publication_candidate_with_state_move_test() {
  let dir = "test/tmp/daemon-dispatch-recovery-reject"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: False,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_with_candidate(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_with_candidate(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      command_runner.Runner(run: fn(_) {
        Error(command_runner.command_error("unexpected_publication_retry"))
      }),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(log_subject, "state_transition:Triage", 100)
  assert wait_for_log(log_subject, "dispatch_recovery_rejected", 100)
  assert !wait_for_log(log_subject, "agent_run:issue-1", 5)
  assert contains_kind_sequence(root, ["issue_parked_v2"])

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn dispatch_recovery_publication_workflow_drift_starts_superseding_run_without_parking_test() {
  let dir = "test/tmp/daemon-dispatch-recovery-publication-workflow-drift"
  let issue = issue("issue-1", "LIV-1402", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  append_auto_unpark_issue_change_parked_record(root, issue, 20)
  drift_retry_publication_workflow(workflow_path)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let base_deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_with_candidate(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_with_candidate(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected agent step")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let deps =
    daemon.RuntimeDependencies(
      ..base_deps,
      workflow_run_dependencies: workflow_run.Dependencies(
        ..base_deps.workflow_run_dependencies,
        command_step: fn(
          context: workflow_run.StepContext,
          _command: String,
          _timeout_ms: Int,
          _secrets: List(String),
          limits: config_types.ArtifactLimits,
        ) {
          let tmp_dir = context.workspace_path <> "/tmp"
          let _ = simplifile.create_directory_all(tmp_dir)
          let _ =
            simplifile.write(
              tmp_dir <> "/commit-stack.json",
              commit_stack_payload(context.run_id),
            )
          step_artifact.from_command_result(
            context.step_id,
            0,
            "seeded",
            "",
            False,
            [],
            limits,
          )
        },
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(
    log_subject,
    "dispatch_recovery_publication_superseded",
    100,
  )
  assert wait_for_log(log_subject, "dispatch_started", 100)
  assert !wait_for_log(log_subject, "dispatch_recovery_rejected", 5)
  assert count_kind(root, "workflow_run_superseded") == 1
  assert count_kind(root, "issue_parked_v2") == 1

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn dispatch_recovery_existing_superseding_run_does_not_start_duplicate_test() {
  let dir = "test/tmp/daemon-dispatch-recovery-publication-superseded"
  let issue = issue("issue-1", "LIV-1406", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  seed_existing_superseding_run(root, issue, "run-1", "run-2", 1030)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_with_candidate(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_with_candidate(issue),
      ),
      hub_subject,
      fn(issue, _, _) {
        process.send(log_subject, "agent_run:" <> issue.id)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(
    log_subject,
    "dispatch_recovery_superseding_run_exists",
    100,
  )
  assert !wait_for_log(log_subject, "dispatch_started", 5)
  assert !wait_for_log(log_subject, "agent_run:issue-1", 5)
  assert count_kind(root, "workflow_run_superseded") == 1
  assert count_kind(root, "issue_parked_v2") == 0

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn dispatch_recovery_skips_recovered_published_run_without_parking_test() {
  let dir = "test/tmp/daemon-dispatch-recovery-publication-published"
  let issue = issue("issue-1", "LIV-1175", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_recovered_published_publication_run(root, issue, "run-1", 1000)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_with_candidate(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_with_candidate(issue),
      ),
      hub_subject,
      fn(issue, _, _) {
        process.send(log_subject, "agent_run:" <> issue.id)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      command_runner.Runner(run: fn(_) {
        Error(command_runner.command_error("unexpected_publication_retry"))
      }),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(log_subject, "dispatch_recovery_already_published", 100)
  assert wait_for_log(log_subject, "state_transition:Done", 100)
  assert !wait_for_log(log_subject, "state_transition:Triage", 5)
  assert !wait_for_log(log_subject, "comment:publication_retry", 5)
  assert !wait_for_log(log_subject, "agent_run:issue-1", 5)
  assert count_kind(root, "issue_parked_v2") == 0

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn dispatch_recovery_reuses_retry_step_on_poll_without_fresh_dispatch_test() {
  let dir = "test/tmp/daemon-dispatch-recovery-step"
  let issue = issue("issue-1", "LIV-1059", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_with_candidate(issue),
      hub_subject,
      fn(issue, _context, effective) {
        process.send(log_subject, "recovered_worker_started:" <> issue.id)
        process.send(
          log_subject,
          recovery_append_state(log_subject, effective.workspace.root),
        )
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(log_subject, "recovered_worker_started:issue-1", 100)
  assert wait_for_log(log_subject, "retry_step_ledger_ready", 100)
  assert !wait_for_log(log_subject, "dispatch_started", 5)
  assert contains_kind_sequence(root, [
    "workflow_repair_requested",
    "step_attempt_superseded",
    "workflow_run_started",
    "known_workspace",
    "issue_counter_updated",
  ])

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn dispatch_recovery_repeated_poll_is_idempotent_while_recovered_run_active_test() {
  let dir = "test/tmp/daemon-dispatch-recovery-step-idempotent"
  let issue = issue("issue-1", "LIV-1059", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_with_candidate(issue),
      hub_subject,
      fn(issue, _context, _effective) {
        process.send(log_subject, "recovered_worker_started:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "recovered_worker_started:issue-1", 100)

  process.send(started.data, daemon.PollTick(2))

  assert !wait_for_log(log_subject, "recovered_worker_started:issue-1", 5)
  assert count_kind(root, "workflow_repair_requested") == 1
  assert count_kind(root, "step_attempt_superseded") == 1

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn dispatch_recovery_reuses_retry_step_for_same_fingerprint_auto_parked_todo_test() {
  let dir = "test/tmp/daemon-dispatch-recovery-step-auto-parked"
  let issue = issue("issue-1", "LIV-1059", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  append_auto_unpark_issue_change_parked_record(root, issue, 20)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_with_candidate(issue),
      hub_subject,
      fn(issue, _context, effective) {
        process.send(log_subject, "recovered_worker_started:" <> issue.id)
        process.send(
          log_subject,
          recovery_append_state(log_subject, effective.workspace.root),
        )
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(log_subject, "recovered_worker_started:issue-1", 100)
  assert wait_for_log(log_subject, "retry_step_ledger_ready", 100)
  assert !wait_for_log(log_subject, "dispatch_started", 5)
  assert contains_kind_sequence(root, [
    "issue_parked_v2",
    "workflow_repair_requested",
    "step_attempt_superseded",
    "workflow_run_started",
    "known_workspace",
    "issue_counter_updated",
  ])

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn dispatch_recovery_same_fingerprint_auto_parked_todo_without_retained_run_starts_fresh_dispatch_test() {
  let dir = "test/tmp/dr-auto-parked-fresh"
  let issue = issue("issue-1", "LIV-1059", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  append_auto_unpark_issue_change_parked_record(root, issue, 20)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let base_deps =
    in_process_dependencies(
      log_subject,
      tracker_with_candidate(issue),
      hub_subject,
      fn(issue, _context, _effective) {
        process.send(log_subject, "agent_run:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let deps =
    daemon.RuntimeDependencies(
      ..base_deps,
      workflow_run_dependencies: workflow_run.Dependencies(
        ..base_deps.workflow_run_dependencies,
        command_step: fn(
          context: workflow_run.StepContext,
          _command: String,
          _timeout_ms: Int,
          _secrets: List(String),
          limits: config_types.ArtifactLimits,
        ) {
          step_artifact.from_command_result(
            context.step_id,
            0,
            "seeded",
            "",
            False,
            [],
            limits,
          )
        },
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(log_subject, "dispatch_started", 100)
  assert wait_for_log(log_subject, "agent_run:issue-1", 100)
  assert count_kind(root, "workflow_repair_requested") == 0
  assert count_kind(root, "step_attempt_superseded") == 0
  assert contains_kind_sequence(root, [
    "issue_parked_v2",
    "outbox_pending_v2",
    "outbox_attempted",
    "workflow_run_started",
    "known_workspace",
    "issue_counter_updated",
  ])

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn dispatch_recovery_same_fingerprint_auto_parked_todo_is_idempotent_test() {
  let dir = "test/tmp/daemon-dispatch-recovery-step-auto-parked-idempotent"
  let issue = issue("issue-1", "LIV-1059", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  append_auto_unpark_issue_change_parked_record(root, issue, 20)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_with_candidate(issue),
      hub_subject,
      fn(issue, _context, _effective) {
        process.send(log_subject, "recovered_worker_started:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("stopped")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "recovered_worker_started:issue-1", 100)

  process.send(started.data, daemon.PollTick(2))

  assert !wait_for_log(log_subject, "recovered_worker_started:issue-1", 5)
  assert count_kind(root, "workflow_repair_requested") == 1
  assert count_kind(root, "step_attempt_superseded") == 1

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn dispatch_recovery_tracker_transition_failure_parks_and_suppresses_repeat_poll_test() {
  let dir = "test/tmp/daemon-dispatch-recovery-transition-failure"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: False,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_with_candidate(issue),
      tracker_adapter_with_failing_transition_logging(
        log_subject,
        tracker_with_candidate(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      command_runner.Runner(run: fn(_) {
        Error(command_runner.command_error("unexpected_publication_retry"))
      }),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(log_subject, "state_transition_failed:Triage", 100)
  assert wait_for_log(
    log_subject,
    "dispatch_recovery_rejected_state_transition_failed",
    100,
  )
  assert wait_for_log(log_subject, "dispatch_recovery_rejected", 100)
  assert contains_kind_sequence(root, ["issue_parked_v2"])

  process.send(started.data, daemon.PollTick(2))

  assert !wait_for_log(log_subject, "state_transition_failed:Triage", 5)
  assert count_kind(root, "issue_parked_v2") == 1

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn dispatch_recovery_publication_retry_honors_workflow_completion_override_test() {
  let dir = "test/tmp/daemon-dispatch-recovery-publication-workflow-override"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) =
    write_retry_publication_workflow_with_task_updates(
      dir,
      "task_updates:\n  enabled: true\n  workflows:\n    execplan:\n      requires_review: false\n      states:\n        no_review_success: Done\n",
    )
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_with_candidate(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_with_candidate(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(log_subject, "comment:publication_retry", 100)
  assert wait_for_log(log_subject, "state_transition:Done", 100)
  assert !wait_for_log(
    log_subject,
    "publication_retry_completion_target_missing",
    5,
  )
  assert !wait_for_log(log_subject, "agent_run:issue-1", 5)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn dispatch_recovery_retries_publication_and_moves_issue_out_of_todo_test() {
  let dir = "test/tmp/daemon-dispatch-recovery-publication"
  let issue = issue("issue-1", "LIV-739", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_with_candidate(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_with_candidate(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(log_subject, "comment:publication_retry", 100)
  assert wait_for_log(log_subject, "state_transition:Done", 100)
  assert !wait_for_log(log_subject, "agent_run:issue-1", 5)

  let attempts =
    projection.publication_attempts_for_run(
      load_projection_or_panic(root),
      "run-1",
      "execplan_review_doc",
    )
  assert list.length(attempts) == 2

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn artifact_publication_retry_queues_operation_before_publication_driver_work_test() {
  let dir = "test/tmp/daemon-artifact-publication-retry-queued"
  let issue = issue("issue-1", "LIV-1264", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let publish_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      fn(_) {
        adapter_legacy.adapter_from_legacy_client(
          tracker_issue_only(issue),
          "linear",
        )
      },
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      blocking_publication_retry_runner(log_subject, publish_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryArtifactPublication("run-1", Some("execplan_review_doc")),
      1000,
    )

  assert command.status_to_string(result.status) == "queued"
  assert result.message
    == Some(
      "artifact publication retry accepted; poll query operation-status for completion",
    )
  let assert Some(operation_id) = result.operation_id
  assert string.starts_with(
    operation_id,
    "artifact-publication-retry:run-1:execplan_review_doc:",
  )
  let assert Ok(operation) =
    projection.control_operation(load_projection_or_panic(root), operation_id)
  assert operation.publication_id == Some("execplan_review_doc")
  assert operation.run_id == Some("run-1")
  assert publication_attempt_count(root, "run-1", "execplan_review_doc") == 1

  assert wait_for_log(log_subject, "publication_driver_started", 100)
  assert publication_attempt_count(root, "run-1", "execplan_review_doc") == 1

  test_async.release_barrier(publish_barrier)
  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.message
    == Some("publication retry recorded execplan_review_doc as published")
  assert publication_attempt_count(root, "run-1", "execplan_review_doc") == 2

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn artifact_publication_retry_reuses_existing_operation_for_duplicate_target_test() {
  let dir = "test/tmp/daemon-artifact-publication-retry-duplicate"
  let issue = issue("issue-1", "LIV-1264", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let publish_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      fn(_) {
        adapter_legacy.adapter_from_legacy_client(
          tracker_issue_only(issue),
          "linear",
        )
      },
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      blocking_publication_retry_runner(log_subject, publish_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(first) =
    daemon.apply_operator_command(
      started.data,
      command.RetryArtifactPublication("run-1", Some("execplan_review_doc")),
      1000,
    )
  let assert Some(first_operation_id) = first.operation_id
  assert wait_for_log(log_subject, "publication_driver_started", 100)

  let assert Ok(second) =
    daemon.apply_operator_command(
      started.data,
      command.RetryArtifactPublication("run-1", Some("execplan_review_doc")),
      1000,
    )
  assert command.status_to_string(second.status) == "queued"
  assert second.operation_id == Some(first_operation_id)
  let assert Some(second_message) = second.message
  assert string.contains(
    second_message,
    "artifact publication retry already queued/running",
  )
  assert string.contains(
    second_message,
    "Next safe command: scripts/scherzoctl query operation-status "
      <> first_operation_id
      <> " --json",
  )
  assert count_kind(root, "control_operation_queued") == 1
  assert publication_attempt_count(root, "run-1", "execplan_review_doc") == 1

  test_async.release_barrier(publish_barrier)
  let assert Ok(completed_operation) =
    wait_for_operation_status(root, first_operation_id, "completed", 20)
  assert completed_operation.publication_id == Some("execplan_review_doc")
  assert publication_attempt_count(root, "run-1", "execplan_review_doc") == 2

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn artifact_publication_retry_rejects_invalid_targets_without_queued_work_test() {
  let dir = "test/tmp/daemon-artifact-publication-retry-rejects"
  let issue = issue("issue-1", "LIV-1264", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      fn(_) {
        adapter_legacy.adapter_from_legacy_client(
          tracker_issue_only(issue),
          "linear",
        )
      },
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      command_runner.Runner(run: fn(_) {
        process.send(log_subject, "unexpected_publication_retry")
        Error(command_runner.command_error("unexpected_publication_retry"))
      }),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(missing_run) =
    daemon.apply_operator_command(
      started.data,
      command.RetryArtifactPublication("missing-run", None),
      1000,
    )
  assert command.status_to_string(missing_run.status) == "not_found"

  let assert Ok(missing_publication) =
    daemon.apply_operator_command(
      started.data,
      command.RetryArtifactPublication("run-1", Some("missing-publication")),
      1000,
    )
  assert command.status_to_string(missing_publication.status) == "not_found"

  seed_non_retryable_failed_publication_attempt(root, "run-1", at_ms: 1030)
  let assert Ok(non_retryable) =
    daemon.apply_operator_command(
      started.data,
      command.RetryArtifactPublication("run-1", Some("execplan_review_doc")),
      1000,
    )
  assert command.status_to_string(non_retryable.status) == "rejected"
  assert command.status_reason(non_retryable.status)
    == Some("publication_not_retryable")
  assert count_kind(root, "control_operation_queued") == 0
  assert publication_attempt_count(root, "run-1", "execplan_review_doc") == 2
  assert !wait_for_log(log_subject, "unexpected_publication_retry", 5)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn artifact_publication_retry_rejects_when_operation_intent_append_fails_test() {
  let dir = "test/tmp/daemon-artifact-publication-retry-append-fails"
  let issue = issue("issue-1", "LIV-1264", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      fn(_) {
        adapter_legacy.adapter_from_legacy_client(
          tracker_issue_only(issue),
          "linear",
        )
      },
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      command_runner.Runner(run: fn(_) {
        process.send(log_subject, "unexpected_publication_retry")
        Error(command_runner.command_error("unexpected_publication_retry"))
      }),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  chmod_path("a-w", ledger_path.current_path)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryArtifactPublication("run-1", Some("execplan_review_doc")),
      1000,
    )

  chmod_path("u+w", ledger_path.current_path)
  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status) == Some("ledger_append_failed")
  assert result.message
    == Some("failed to append artifact publication retry operation")
  assert count_kind(root, "control_operation_queued") == 0
  assert publication_attempt_count(root, "run-1", "execplan_review_doc") == 1
  assert !wait_for_log(log_subject, "unexpected_publication_retry", 5)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn artifact_publication_retry_async_failure_records_failed_operation_test() {
  let dir = "test/tmp/daemon-artifact-publication-retry-async-failure"
  let issue = issue("issue-1", "LIV-1264", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      fn(_) {
        adapter_legacy.adapter_from_legacy_client(
          tracker_issue_only(issue),
          "linear",
        )
      },
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      failing_publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryArtifactPublication("run-1", Some("execplan_review_doc")),
      1000,
    )
  let assert Some(operation_id) = result.operation_id
  let assert Ok(failed_operation) =
    wait_for_operation_status(root, operation_id, "failed", 20)
  assert failed_operation.reason == Some("publication_retry_attempt_failed")
  let assert Some(message) = failed_operation.message
  assert string.contains(message, "workspace_driver_publish_failed")
  assert string.contains(message, "driver exploded")
  let assert Ok(latest) =
    projection.latest_publication_for_run(
      load_projection_or_panic(root),
      "run-1",
      "execplan_review_doc",
    )
  assert latest.status == "failed"
  assert latest.error_code == Some("workspace_driver_publish_failed")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn artifact_publication_retry_startup_replay_replays_queued_and_skips_completed_test() {
  let dir = "test/tmp/daemon-artifact-publication-retry-startup-replay"
  let issue = issue("issue-1", "LIV-1264", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_failed_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let operation_id =
    "artifact-publication-retry:run-1:execplan_review_doc:queued-replay"
  append_ledger_records(root, [
    record.with_id(
      "publication-retry-queued-op",
      1040,
      record.ControlOperationQueued(
        operation_id: operation_id,
        operation_kind: "artifact_publication_retry",
        command_name: "retry_artifact_publication",
        target: "run:run-1:execplan_review_doc",
        run_id: Some("run-1"),
        issue_id: Some(issue.id),
        issue_identifier: Some(issue.identifier),
        requested_step_id: None,
        publication_id: Some("execplan_review_doc"),
      ),
    ),
  ])
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      fn(_) {
        adapter_legacy.adapter_from_legacy_client(
          tracker_issue_only(issue),
          "linear",
        )
      },
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.publication_id == Some("execplan_review_doc")
  assert publication_attempt_count(root, "run-1", "execplan_review_doc") == 2
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)

  let skip_log_subject = process.new_subject()
  let skip_deps =
    in_process_dependencies_with_adapter(
      skip_log_subject,
      tracker_issue_only(issue),
      fn(_) {
        adapter_legacy.adapter_from_legacy_client(
          tracker_issue_only(issue),
          "linear",
        )
      },
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      command_runner.Runner(run: fn(_) {
        process.send(skip_log_subject, "unexpected_completed_replay")
        Error(command_runner.command_error("unexpected_completed_replay"))
      }),
    )
  let assert Ok(skip_started) = daemon.start(Some(workflow_path), skip_deps)
  let assert Ok(Nil) =
    daemon.await_startup_recovery_ready(skip_started.data, 1000)
  assert !wait_for_log(skip_log_subject, "unexpected_completed_replay", 20)
  assert publication_attempt_count(root, "run-1", "execplan_review_doc") == 2

  assert daemon.shutdown(skip_started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

fn issue(id: String, identifier: String, state: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: None,
    state: issue_state.from_string_unchecked(state),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: None,
    updated_at: None,
  )
}

fn tracker_with_candidate(candidate: tracker_issue.Issue) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([candidate]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
  )
}

fn tracker_issue_only(candidate: tracker_issue.Issue) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
  )
}

fn tracker_issue_only_blocking_retry_operation_fetch(
  candidate: tracker_issue.Issue,
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
  gate: process.Subject(FetchCounterMessage),
) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) {
      case should_block_fetch(gate) {
        True -> {
          process.send(log_subject, "retry_step_operation_fetch_blocked")
          test_async.wait_at_barrier(barrier)
          Ok([candidate])
        }
        False -> Ok([candidate])
      }
    },
  )
}

fn start_fetch_counter() -> process.Subject(FetchCounterMessage) {
  let reply = process.new_subject()
  let _pid =
    process.spawn(fn() {
      let subject = process.new_subject()
      process.send(reply, subject)
      fetch_counter_loop(subject, False, False)
    })
  process.receive_forever(reply)
}

fn arm_fetch_gate(gate: process.Subject(FetchCounterMessage)) -> Nil {
  process.send(gate, ArmFetchGate)
}

fn should_block_fetch(gate: process.Subject(FetchCounterMessage)) -> Bool {
  let reply = process.new_subject()
  process.send(gate, FetchShouldBlock(reply))
  process.receive_forever(reply)
}

fn fetch_counter_loop(
  subject: process.Subject(FetchCounterMessage),
  armed: Bool,
  consumed: Bool,
) -> Nil {
  case process.receive_forever(subject) {
    ArmFetchGate -> fetch_counter_loop(subject, True, consumed)
    FetchShouldBlock(reply) -> {
      let #(block, armed, consumed) =
        fetch_gate_decision(subject, armed, consumed)
      process.send(reply, block)
      fetch_counter_loop(subject, armed, consumed)
    }
  }
}

fn fetch_gate_decision(
  _subject: process.Subject(FetchCounterMessage),
  armed: Bool,
  consumed: Bool,
) -> #(Bool, Bool, Bool) {
  case consumed {
    True -> #(False, armed, consumed)
    False ->
      case armed {
        True -> #(True, False, True)
        False -> #(False, False, False)
      }
  }
}

fn run_finalize_command(
  run_id: String,
  dry_run dry_run: Bool,
) -> command.OperatorCommand {
  command.RunFinalize(
    run_id: run_id,
    validate: True,
    outputs: command.RunFinalizeOutputsAuto,
    publish: True,
    update_tracker: True,
    dry_run: dry_run,
    reason: "operator salvage",
    allow_unpublished: False,
  )
}

fn run_finalize_command_allow_unpublished(
  run_id: String,
  dry_run dry_run: Bool,
) -> command.OperatorCommand {
  command.RunFinalize(
    run_id: run_id,
    validate: True,
    outputs: command.RunFinalizeOutputsAuto,
    publish: True,
    update_tracker: True,
    dry_run: dry_run,
    reason: "operator salvage",
    allow_unpublished: True,
  )
}

type IssueSequenceMessage {
  IssueSequenceSet(issue: tracker_issue.Issue, reply: process.Subject(Nil))
  IssueSequenceNext(reply: process.Subject(tracker_issue.Issue))
  IssueSequenceStop
}

fn start_issue_sequence(
  initial: tracker_issue.Issue,
) -> process.Subject(IssueSequenceMessage) {
  let name = process.new_name("issue-sequence")
  let ready = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let assert Ok(Nil) = process.register(process.self(), name)
      let subject = process.named_subject(name)
      process.send(ready, Nil)
      issue_sequence_loop(subject, initial)
    })
  let assert Ok(Nil) = process.receive(ready, within: 1000)
  process.named_subject(name)
}

fn issue_sequence_loop(
  subject: process.Subject(IssueSequenceMessage),
  current: tracker_issue.Issue,
) -> Nil {
  case process.receive_forever(subject) {
    IssueSequenceSet(issue, reply) -> {
      process.send(reply, Nil)
      issue_sequence_loop(subject, issue)
    }
    IssueSequenceNext(reply) -> {
      process.send(reply, current)
      issue_sequence_loop(subject, current)
    }
    IssueSequenceStop -> Nil
  }
}

fn set_issue_sequence(
  subject: process.Subject(IssueSequenceMessage),
  issue: tracker_issue.Issue,
) -> Nil {
  let reply = process.new_subject()
  process.send(subject, IssueSequenceSet(issue, reply))
  let assert Ok(Nil) = process.receive(reply, within: 1000)
  Nil
}

fn tracker_issue_sequence(
  subject: process.Subject(IssueSequenceMessage),
) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) {
      let reply = process.new_subject()
      process.send(subject, IssueSequenceNext(reply))
      case process.receive(reply, within: 1000) {
        Ok(issue) -> Ok([issue])
        Error(Nil) -> Ok([])
      }
    },
  )
}

fn in_process_dependencies(
  log_subject: process.Subject(String),
  tracker_client: tracker.Client,
  hub_subject: process.Subject(hub.Message),
  agent_runner: fn(
    tracker_issue.Issue,
    workflow_run.StepContext,
    config_types.EffectiveConfig,
  ) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> daemon.RuntimeDependencies {
  in_process_dependencies_with_adapter(
    log_subject,
    tracker_client,
    fn(_) {
      let legacy =
        adapter_legacy.adapter_from_legacy_client(tracker_client, "linear")
      adapter.TrackerAdapter(
        ..legacy,
        handoff: Some(test_handoff_capability(disabled_handoff())),
      )
    },
    hub_subject,
    agent_runner,
    command_runner.Runner(run: fn(_) {
      Error(command_runner.command_error("unexpected_publication_retry"))
    }),
  )
}

pub fn run_finalize_dry_run_reports_plan_without_queueing_test() {
  let dir = "test/tmp/daemon-run-finalize-dry-run"
  let issue = issue("issue-1", "LIV-1336", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_interrupted_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let worker_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_issue_only(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        process.send(worker_subject, "unexpected_worker")
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RunFinalize(
        run_id: "run-1",
        validate: True,
        outputs: command.RunFinalizeOutputsAuto,
        publish: True,
        update_tracker: True,
        dry_run: True,
        reason: "operator salvage",
        allow_unpublished: False,
      ),
      1000,
    )

  assert command.status_to_string(result.status) == "applied"
  let assert Some(message) = result.message
  assert string.contains(message, "would validate retained evidence")
  assert count_kind(root, "control_operation_queued") == 0
  test_async.assert_no_extra_message(worker_subject)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_finalize_dry_run_reports_declared_pending_publication_for_finished_run_test() {
  let dir = "test/tmp/daemon-run-finalize-finished-declared-dry-run"
  let issue = issue("issue-1", "LIV-1336", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_finished_publication_run_without_attempts(root, issue, "run-1", 1000)
  let log_subject = process.new_subject()
  let worker_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_issue_only(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        process.send(worker_subject, "unexpected_worker")
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      run_finalize_command("run-1", dry_run: True),
      1000,
    )

  assert command.status_to_string(result.status) == "applied"
  let assert Some(message) = result.message
  assert string.contains(message, "execplan_review_doc=pending(required)")
  assert !string.contains(message, "no publication targets")
  assert count_kind(root, "control_operation_queued") == 0
  test_async.assert_no_extra_message(worker_subject)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_finalize_finished_run_queues_declared_publication_retry_test() {
  let dir = "test/tmp/daemon-run-finalize-finished-declared-queue"
  let issue = issue("issue-1", "LIV-1336", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_finished_publication_run_without_attempts(root, issue, "run-1", 1000)
  let log_subject = process.new_subject()
  let worker_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_issue_only(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        process.send(worker_subject, "unexpected_worker")
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      run_finalize_command("run-1", dry_run: False),
      1000,
    )

  assert command.status_to_string(result.status) == "queued"
  let assert Some(operation_id) = result.operation_id
  assert string.starts_with(
    operation_id,
    "artifact-publication-retry:run-1:all:",
  )
  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.message
    == Some("publication retry recorded execplan_review_doc as published")
  assert publication_attempt_count(root, "run-1", "execplan_review_doc") == 1
  assert count_kind(root, "workflow_run_finished") == 1
  test_async.assert_no_extra_message(worker_subject)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_finalize_dry_run_rejects_parked_issue_test() {
  let dir = "test/tmp/daemon-run-finalize-parked-issue"
  let issue = issue("issue-1", "LIV-1336", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_interrupted_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append(
      ledger_path,
      record.with_id(
        "issue-parked",
        1010,
        record.IssueParkedV2(
          issue_id: issue.id,
          issue_identifier: issue.identifier,
          reason: "manual_hold",
          release_policy: "explicit_unpark_only",
          issue_fingerprint: core.issue_fingerprint(issue),
          observed_updated_at_ms: 1010,
        ),
      ),
      True,
    )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_issue_only(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      run_finalize_command("run-1", dry_run: True),
      1000,
    )

  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status) == Some("issue_parked")
  assert count_kind(root, "control_operation_queued") == 0

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_finalize_dry_run_rejects_non_active_issue_state_test() {
  let dir = "test/tmp/daemon-run-finalize-non-active-issue"
  let current_issue = issue("issue-1", "LIV-1336", "Backlog")
  let seeded_issue = issue("issue-1", "LIV-1336", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_interrupted_publication_retry_run(
    root,
    seeded_issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(current_issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_issue_only(current_issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      run_finalize_command("run-1", dry_run: True),
      1000,
    )

  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status)
    == Some("issue_state_drift:non_active_state")
  assert count_kind(root, "control_operation_queued") == 0

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_finalize_dry_run_rejects_superseded_run_test() {
  let dir = "test/tmp/daemon-run-finalize-superseded"
  let issue = issue("issue-1", "LIV-1336", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_interrupted_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append(
      ledger_path,
      record.with_id(
        "workflow-superseded-run-1",
        1030,
        record.WorkflowRunSuperseded(
          run_id: "run-1",
          workflow_id: "execplan",
          issue_id: issue.id,
          superseded_by_run_id: "run-2",
          reason: "newer_run_started",
        ),
      ),
      True,
    )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_issue_only(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      run_finalize_command("run-1", dry_run: True),
      1000,
    )

  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status) == Some("run_superseded")
  assert count_kind(root, "control_operation_queued") == 0

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_finalize_queues_operation_updates_tracker_and_is_idempotent_test() {
  let dir = "test/tmp/daemon-run-finalize-queued"
  let issue = issue("issue-1", "LIV-1336", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_interrupted_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let worker_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_issue_only(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        process.send(worker_subject, "unexpected_worker")
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let operator_command =
    command.RunFinalize(
      run_id: "run-1",
      validate: True,
      outputs: command.RunFinalizeOutputsAuto,
      publish: True,
      update_tracker: True,
      dry_run: False,
      reason: "operator salvage",
      allow_unpublished: True,
    )
  let assert Ok(first) =
    daemon.apply_operator_command(started.data, operator_command, 1000)
  assert command.status_to_string(first.status) == "queued"
  let assert Some(operation_id) = first.operation_id
  assert string.starts_with(operation_id, "run-finalize:run-1:")
  assert count_kind(root, "control_operation_queued") == 1

  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.message
    == Some("run finalize completed without starting a worker")
  assert publication_attempt_count(root, "run-1", "execplan_review_doc") == 1
  assert count_kind(root, "workflow_run_finished") == 1
  assert wait_for_log(log_subject, "state_transition:Done", 100)
  test_async.assert_no_extra_message(worker_subject)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_finalize_rejects_queue_append_failure_without_async_work_test() {
  let dir = "test/tmp/daemon-run-finalize-queue-append-failed"
  let issue = issue("issue-1", "LIV-1336", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_interrupted_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_issue_only(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  chmod_path("a-w", ledger_path.current_path)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      run_finalize_command_allow_unpublished("run-1", dry_run: False),
      1000,
    )

  chmod_path("u+w", ledger_path.current_path)
  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status) == Some("ledger_append_failed")
  assert result.message == Some("failed to append run finalize operation")
  assert count_kind(root, "control_operation_queued") == 0

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_finalize_dry_run_rejects_issue_drift_test() {
  let dir = "test/tmp/daemon-run-finalize-issue-drift"
  let issue = issue("issue-1", "LIV-1336", "Todo")
  let changed_issue = tracker_issue.Issue(..issue, title: "Changed title")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_interrupted_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(changed_issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_issue_only(changed_issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RunFinalize(
        run_id: "run-1",
        validate: True,
        outputs: command.RunFinalizeOutputsAuto,
        publish: True,
        update_tracker: True,
        dry_run: True,
        reason: "operator salvage",
        allow_unpublished: False,
      ),
      1000,
    )

  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status) == Some("issue_drift")
  assert count_kind(root, "control_operation_queued") == 0

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_finalize_dry_run_rejects_output_recovery_failure_test() {
  let dir = "test/tmp/daemon-run-finalize-output-recovery-failure"
  let issue = issue("issue-1", "LIV-1336", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_interrupted_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: False,
  )
  let assert Ok(Nil) = simplifile.delete(root <> "/runs/run-1/workspaces/main")
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_issue_only(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RunFinalize(
        run_id: "run-1",
        validate: True,
        outputs: command.RunFinalizeOutputsAuto,
        publish: True,
        update_tracker: True,
        dry_run: True,
        reason: "operator salvage",
        allow_unpublished: False,
      ),
      1000,
    )

  assert command.status_to_string(result.status) == "rejected"
  let assert Some(reason) = command.status_reason(result.status)
  assert reason == "run_not_complete" || reason == "workspace_recovery_failed"
  assert count_kind(root, "control_operation_queued") == 0

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_finalize_rejects_unpublished_required_publication_without_async_work_test() {
  let dir = "test/tmp/daemon-run-finalize-publication-pending"
  let issue = issue("issue-1", "LIV-1336", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_interrupted_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_issue_only(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RunFinalize(
        run_id: "run-1",
        validate: True,
        outputs: command.RunFinalizeOutputsAuto,
        publish: True,
        update_tracker: True,
        dry_run: False,
        reason: "operator salvage",
        allow_unpublished: False,
      ),
      1000,
    )
  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status) == Some("publication_pending")
  assert result.operation_id == None
  assert count_kind(root, "control_operation_queued") == 0
  assert count_kind(root, "workflow_run_finished") == 0
  assert !wait_for_log(log_subject, "state_transition:Done", 5)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_finalize_async_tracker_update_failure_records_failed_operation_test() {
  let dir = "test/tmp/daemon-run-finalize-tracker-update-failure"
  let issue = issue("issue-1", "LIV-1336", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_interrupted_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      tracker_adapter_with_failing_transition_logging(
        log_subject,
        tracker_issue_only(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RunFinalize(
        run_id: "run-1",
        validate: True,
        outputs: command.RunFinalizeOutputsAuto,
        publish: True,
        update_tracker: True,
        dry_run: False,
        reason: "operator salvage",
        allow_unpublished: True,
      ),
      1000,
    )
  let assert Some(operation_id) = result.operation_id
  let assert Ok(failed_operation) =
    wait_for_operation_status(root, operation_id, "failed", 20)
  assert failed_operation.reason == Some("tracker_update_failed")
  assert wait_for_log(log_subject, "state_transition_failed:Done", 100)
  assert count_kind(root, "workflow_run_finished") == 0

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn run_finalize_startup_replay_replays_queued_operation_test() {
  let dir = "test/tmp/daemon-run-finalize-startup-replay"
  let issue = issue("issue-1", "LIV-1336", "Todo")
  let #(workflow_path, root) = write_retry_publication_workflow(dir)
  seed_interrupted_publication_retry_run(
    root,
    issue,
    "run-1",
    1000,
    include_output_manifest: True,
  )
  let operation_id = "run-finalize:run-1:queued-replay"
  append_ledger_records(root, [
    record.with_id(
      "queued-op",
      40,
      record.ControlOperationQueued(
        operation_id: operation_id,
        operation_kind: "run_finalize",
        command_name: "run_finalize_allow_unpublished",
        target: "run:run-1",
        run_id: Some("run-1"),
        issue_id: Some(issue.id),
        issue_identifier: Some(issue.identifier),
        requested_step_id: None,
        publication_id: None,
      ),
    ),
  ])
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies_with_adapter(
      log_subject,
      tracker_issue_only(issue),
      tracker_adapter_with_transition_logging(
        log_subject,
        tracker_issue_only(issue),
      ),
      hub_subject,
      fn(_, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unexpected spawn")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
      publication_retry_runner(),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 1000)

  let assert Ok(completed_operation) =
    wait_for_operation_status(root, operation_id, "completed", 20)
  assert completed_operation.message
    == Some("run finalize completed without starting a worker")
  assert count_kind(root, "workflow_run_finished") == 1

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

fn in_process_dependencies_with_adapter(
  log_subject: process.Subject(String),
  _tracker_client: tracker.Client,
  make_tracker_adapter: fn(config_types.EffectiveConfig) ->
    adapter.TrackerAdapter,
  hub_subject: process.Subject(hub.Message),
  agent_runner: fn(
    tracker_issue.Issue,
    workflow_run.StepContext,
    config_types.EffectiveConfig,
  ) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
  publication_runner: command_runner.Runner,
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..daemon.default_dependencies(),
    make_tracker_adapter: fn(effective) {
      make_tracker_adapter(effective)
      |> ensure_state_transition_capability
    },
    cleanup: fn(_, _, _) { Ok(Nil) },
    logger: fn(_, event, _fields, _) {
      process.send(log_subject, event)
      Ok(Nil)
    },
    now_ms: fn() { 42 },
    send_after: fn(_, delay, _) { daemon.TestTimer(delay) },
    cancel_timer: fn(_) { Nil },
    workflow_run_dependencies: workflow_run.Dependencies(
      ..workflow_run.default_dependencies(),
      agent_step: fn(issue, context, _, _, effective, _, _, _, _) {
        agent_runner(issue, context, effective)
      },
    ),
    publication_command_runner: publication_runner,
    start_event_hub: fn() { Ok(hub_subject) },
    make_control_token: fn() { Ok("test-token") },
    start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
    stop_control_server: fn(_) { Nil },
  )
}

fn ensure_state_transition_capability(
  tracker_adapter: adapter.TrackerAdapter,
) -> adapter.TrackerAdapter {
  case tracker_adapter.state_transitions {
    Some(_) -> tracker_adapter
    None ->
      adapter.TrackerAdapter(
        ..tracker_adapter,
        state_transitions: Some(
          adapter.StateTransitionCapability(transition: fn(request) {
            Ok(adapter.StateTransitionReceipt(
              task: request.task,
              state: task.TaskState(
                id: None,
                name: request.target_state_name,
                category: task.Ready,
              ),
            ))
          }),
        ),
      )
  }
}

fn tracker_adapter_with_transition_logging(
  log_subject: process.Subject(String),
  tracker_client: tracker.Client,
) -> fn(config_types.EffectiveConfig) -> adapter.TrackerAdapter {
  fn(_) {
    let legacy =
      adapter_legacy.adapter_from_legacy_client(tracker_client, "linear")
    adapter.TrackerAdapter(
      ..legacy,
      comments: Some(
        adapter.CommentCapability(
          post_or_update: fn(request) {
            let adapter.CommentRequest(body: body, ..) = request
            case string.contains(body, "retained publication output") {
              True -> process.send(log_subject, "comment:publication_retry")
              False -> process.send(log_subject, "comment:other")
            }
            Ok(adapter.CommentReceipt(
              id: "comment-1",
              task: request.task,
              url: None,
              created: True,
            ))
          },
          find_by_marker: fn(_) { Ok(None) },
        ),
      ),
      state_transitions: Some(
        adapter.StateTransitionCapability(transition: fn(request) {
          process.send(
            log_subject,
            "state_transition:" <> request.target_state_name,
          )
          Ok(adapter.StateTransitionReceipt(
            task: request.task,
            state: task.TaskState(
              id: None,
              name: request.target_state_name,
              category: task.Ready,
            ),
          ))
        }),
      ),
      handoff: Some(test_handoff_capability(disabled_handoff())),
    )
  }
}

fn tracker_adapter_with_failing_transition_logging(
  log_subject: process.Subject(String),
  tracker_client: tracker.Client,
) -> fn(config_types.EffectiveConfig) -> adapter.TrackerAdapter {
  fn(_) {
    let legacy =
      adapter_legacy.adapter_from_legacy_client(tracker_client, "linear")
    adapter.TrackerAdapter(
      ..legacy,
      state_transitions: Some(
        adapter.StateTransitionCapability(transition: fn(request) {
          process.send(
            log_subject,
            "state_transition_failed:" <> request.target_state_name,
          )
          Error(adapter.Permanent("transition failed"))
        }),
      ),
      handoff: Some(test_handoff_capability(disabled_handoff())),
    )
  }
}

fn disabled_handoff() -> handoff.Client {
  handoff.disabled_client()
}

fn test_handoff_capability(
  client: handoff.Client,
) -> adapter.HandoffCapability {
  adapter.HandoffCapability(report: fn(event) {
    case event {
      adapter.HandoffClaim(task_context, _, run_id) ->
        map_tracker_nil(client.claim_issue(
          task.to_runtime_issue(task_context),
          run_id,
        ))
      adapter.HandoffSuccess(task_context, success, run_id, workflow_id) ->
        map_tracker_nil(client.report_success_for_workflow(
          task.to_runtime_issue(task_context),
          success,
          run_id,
          workflow_id,
        ))
      adapter.HandoffFailure(task_context, failure, run_id, workflow_id) ->
        map_tracker_nil(client.report_failure_for_workflow(
          task.to_runtime_issue(task_context),
          failure,
          run_id,
          workflow_id,
        ))
      adapter.HandoffPark(report) ->
        map_tracker_nil(
          client.report_park(handoff.ParkReport(
            issue_id: report.task.remote_id,
            issue_identifier: report.issue_identifier,
            reason: report.reason,
            release_policy: report.release_policy,
            run_id: report.run_id,
          )),
        )
    }
  })
}

fn map_tracker_nil(
  result: Result(Nil, error.TrackerError),
) -> Result(Nil, adapter.TrackerError) {
  case result {
    Ok(Nil) -> Ok(Nil)
    Error(error.LinearApiRequest(message)) -> Error(adapter.Permanent(message))
    Error(_) -> Error(adapter.Permanent("tracker error"))
  }
}

fn observation_for(
  config_path: String,
  issue: tracker_issue.Issue,
) -> recovery.CurrentWorkflowObservation {
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  startup_recovery.current_workflow_observation(bundle, issue)
}

fn load_projection_or_panic(root: String) -> projection.Projection {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(projected) = ledger.load_projection(ledger_path)
  projected
}

fn write_retry_publication_workflow(dir: String) -> #(String, String) {
  write_retry_publication_workflow_with_task_updates(
    dir,
    retry_publication_task_updates_yaml(),
  )
}

fn write_retry_multi_publication_workflow(dir: String) -> #(String, String) {
  write_retry_publication_workflow_with_publications(
    dir,
    retry_publication_task_updates_yaml(),
    multi_publication_routes_yaml(),
  )
}

fn write_retry_publication_workflow_with_task_updates(
  dir: String,
  task_updates_yaml: String,
) -> #(String, String) {
  write_retry_publication_workflow_with_publications(
    dir,
    task_updates_yaml,
    single_publication_routes_yaml(),
  )
}

fn retry_publication_task_updates_yaml() -> String {
  "task_updates:\n  enabled: true\n  states:\n    success: Done\n    failure: Triage\n"
}

fn write_retry_publication_workflow_with_publications(
  dir: String,
  task_updates_yaml: String,
  publication_routes_yaml: String,
) -> #(String, String) {
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let assert Ok(base) = path.dirname(root)
  let workflow_dir = base <> "/workflows"
  let template_dir = workflow_dir <> "/templates"
  let script_dir = base <> "/scripts"
  let config_path = base <> "/scherzo.yaml"
  test_helpers.reset_dir(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(template_dir)
  let assert Ok(Nil) = simplifile.create_directory_all(script_dir)
  let driver_path = script_dir <> "/retained-driver"
  let assert Ok(Nil) =
    simplifile.write(
      driver_path,
      "#!/bin/sh\nif [ \"$1\" = describe ] && [ \"$2\" = --json ]; then\n  printf '%s\\n' '{\"version\":1,\"capabilities\":[\"publish-commit-stack\"]}'\n  exit 0\nfi\nif [ \"$1\" = lifecycle ]; then\n  if [ \"$2\" = create ]; then\n    mkdir -p \"$SCHERZO_WORKSPACE_PATH\"\n  fi\n  exit 0\nfi\nexit 1\n",
    )
  test_helpers.chmod_executable(driver_path)
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/execplan.yaml",
      "version: 1\nid: execplan\ncontract:\n  version: 1\n  outputs:\n    commit_stack:\n      type: commit_stack\n      source:\n        step: materialize\n        path: tmp/commit-stack.json\nartifacts:\n  publications:\n"
        <> publication_routes_yaml
        <> "steps:\n  - id: materialize\n    kind: command\n    run: ignored\n",
    )
  let assert Ok(Nil) =
    simplifile.write(template_dir <> "/publication.md", "Published by Scherzo.")
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  linear:\n    api_key_env: HOME\n    project: TEST\n  states:\n    ready: [Todo]\n    active: [Todo]\n    terminal: [Done]\nworkspace:\n  root: "
        <> root
        <> "\n  driver: retained\n  drivers:\n    retained:\n      type: custom\n      command: scripts/retained-driver\n      timeout: 1234ms\n"
        <> task_updates_yaml
        <> "agents:\n  concurrency: 1\n  sessions_per_task: 1\n  runtime:\n    type: pi\n    pi:\n      executable: fake\ntask_routing:\n  labels:\n    require_exactly_one: false\n    default_workflow: execplan\nartifacts:\n  repositories:\n    github:\n      docs:\n        repo: scherzo-systems/scherzo\n        base: main\n        branch:\n          strategy: stable_per_work\n          template: scherzo/workflow.{{ workflow.id }}/{{ work.identifier }}/{{ publication.id }}\n        pull_request:\n          enabled: true\n          strategy: update_existing\n          draft: true\n          title: '{{ work.identifier }} publication'\n          body_template: templates/publication.md\nworkflows:\n  execplan: workflows/execplan.yaml\n",
    )
  #(config_path, root)
}

fn drift_retry_publication_workflow(config_path: String) -> Nil {
  let assert Ok(base) = path.dirname(config_path)
  let workflow_path = base <> "/workflows/execplan.yaml"
  let assert Ok(Nil) =
    simplifile.write(
      workflow_path,
      "version: 1\nid: execplan\nsteps:\n  - id: materialize\n    kind: command\n    run: drifted\n",
    )
  Nil
}

fn single_publication_routes_yaml() -> String {
  "    - id: execplan_review_doc\n      repository: github.docs\n      required: true\n      mode: commit_stack\n      pull_request:\n        title: '{{ work.identifier }} publication'\n        body_template: templates/publication.md\n      commit_stack:\n        select:\n          output: commit_stack\n      target:\n        kind: stable_branch\n"
}

fn multi_publication_routes_yaml() -> String {
  single_publication_routes_yaml()
  <> "    - id: execplan_supporting_doc\n      repository: github.docs\n      required: true\n      mode: commit_stack\n      pull_request:\n        title: '{{ work.identifier }} supporting publication'\n        body_template: templates/publication.md\n      commit_stack:\n        select:\n          output: commit_stack\n      target:\n        kind: stable_branch\n"
  <> "    - id: execplan_optional_note\n      repository: github.docs\n      required: false\n      mode: commit_stack\n      pull_request:\n        title: '{{ work.identifier }} optional publication'\n        body_template: templates/publication.md\n      commit_stack:\n        select:\n          output: commit_stack\n      target:\n        kind: stable_branch\n"
}

type SeedPublicationAttempt {
  SeedPublicationAttempt(publication_id: String, status: String, required: Bool)
}

fn seed_publication_attempt(
  publication_id: String,
  status: String,
  required required: Bool,
) -> SeedPublicationAttempt {
  SeedPublicationAttempt(publication_id, status, required)
}

fn seed_recovered_publication_attempts_run(
  root: String,
  issue: tracker_issue.Issue,
  run_id: String,
  at_ms: Int,
  attempts: List(SeedPublicationAttempt),
) -> Nil {
  write_seed_artifact(
    root,
    run_output_ref(run_id),
    commit_stack_payload(run_id),
  )
  write_seed_artifact(root, run_bundle_ref(run_id), "bundle")
  write_publication_retained_workspace_manifest(root, run_id)
  let config_path = publication_config_path(root)
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, workflow)) =
    runtime_bundle.workflow_by_id(bundle, "execplan")
  let assert Ok(fingerprint) =
    workflow_fingerprint_module.fingerprint_for_execution(
      workflow,
      bundle.orchestrator,
    )
  let publication_records =
    list.index_map(attempts, fn(attempt, index) {
      seed_publication_attempt_record(run_id, at_ms + 50 + index, attempt)
    })
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      list.append(
        recovered_publication_run_records(
          root,
          issue,
          run_id,
          at_ms,
          fingerprint,
        ),
        publication_records,
      ),
      True,
    )
  Nil
}

fn recovered_publication_run_records(
  root: String,
  issue: tracker_issue.Issue,
  run_id: String,
  at_ms: Int,
  fingerprint: String,
) -> List(record.LedgerRecord) {
  [
    record.with_id(
      "workflow-started-" <> run_id,
      at_ms,
      record.WorkflowRunStarted(
        run_id: run_id,
        workflow_id: "execplan",
        workflow_fingerprint: fingerprint,
        issue_id: issue.id,
        issue_identifier: issue.identifier,
        issue_fingerprint: tracker_issue.content_fingerprint(issue),
        observed_updated_at_ms: at_ms - 1,
        run_root: root <> "/runs/" <> run_id,
      ),
    ),
    record.with_id(
      "workflow-failed-before-recovery-" <> run_id,
      at_ms + 10,
      record.WorkflowRunFinished(
        run_id: run_id,
        workflow_id: "execplan",
        issue_id: issue.id,
        outcome: "failed_fatal",
        token_total: 0,
        turns: 1,
      ),
    ),
    record.with_id(
      "workflow-repair-requested-" <> run_id,
      at_ms + 20,
      record.WorkflowRepairRequested(
        run_id: run_id,
        workflow_id: "execplan",
        issue_id: issue.id,
        issue_identifier: issue.identifier,
        requested_target: run_id,
        requested_step_id: Some("final_validate"),
        selected_step_id: "final_validate",
        failed_attempt_index: 2,
        next_attempt_index: 3,
        reason: "retry-step",
      ),
    ),
    record.with_id(
      "workflow-recovered-started-" <> run_id,
      at_ms + 30,
      record.WorkflowRunStarted(
        run_id: run_id,
        workflow_id: "execplan",
        workflow_fingerprint: fingerprint,
        issue_id: issue.id,
        issue_identifier: issue.identifier,
        issue_fingerprint: tracker_issue.content_fingerprint(issue),
        observed_updated_at_ms: at_ms - 1,
        run_root: root <> "/runs/" <> run_id,
      ),
    ),
    seeded_output_manifest_record(root, run_id),
    record.with_id(
      "workflow-recovered-finished-" <> run_id,
      at_ms + 40,
      record.WorkflowRunFinished(
        run_id: run_id,
        workflow_id: "execplan",
        issue_id: issue.id,
        outcome: "succeeded_after_recovery",
        token_total: 0,
        turns: 1,
      ),
    ),
  ]
}

fn seed_publication_attempt_record(
  run_id: String,
  at_ms: Int,
  attempt: SeedPublicationAttempt,
) -> record.LedgerRecord {
  let SeedPublicationAttempt(publication_id, status, required) = attempt
  record.with_id(
    "publication-" <> publication_id <> "-" <> run_id,
    at_ms,
    record.PublicationAttemptRecorded(
      run_id: run_id,
      workflow_id: "execplan",
      publication_id: publication_id,
      series_id: "series-" <> publication_id,
      attempt_id: status <> "-" <> publication_id,
      status: status,
      required: required,
      retryable: False,
      retry_execution_available: False,
      version_id: Some("version-" <> publication_id),
      manifest_ref: None,
      manifest_sha256: None,
      manifest_bytes: None,
      error_code: publication_seed_error_code(status),
      error_message: publication_seed_error_message(status),
    ),
  )
}

fn publication_seed_error_code(status: String) -> Option(String) {
  case status {
    "failed" -> Some("publication_not_retryable")
    _ -> None
  }
}

fn publication_seed_error_message(status: String) -> Option(String) {
  case status {
    "failed" -> Some("publication failure cannot be retried")
    _ -> None
  }
}

fn seed_finished_publication_run_without_attempts(
  root: String,
  issue: tracker_issue.Issue,
  run_id: String,
  at_ms: Int,
) -> Nil {
  seed_finished_publication_run_without_attempts_snapshot(
    root,
    issue,
    run_id,
    at_ms,
    include_snapshot: True,
  )
}

fn seed_finished_publication_run_without_attempts_snapshot(
  root: String,
  issue: tracker_issue.Issue,
  run_id: String,
  at_ms: Int,
  include_snapshot include_snapshot: Bool,
) -> Nil {
  write_seed_artifact(
    root,
    run_output_ref(run_id),
    commit_stack_payload(run_id),
  )
  write_seed_artifact(root, run_bundle_ref(run_id), "bundle")
  write_publication_retained_workspace_manifest(root, run_id)
  let config_path = publication_config_path(root)
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, workflow)) =
    runtime_bundle.workflow_by_id(bundle, "execplan")
  let assert Ok(fingerprint) =
    workflow_fingerprint_module.fingerprint_for_execution(
      workflow,
      bundle.orchestrator,
    )
  let snapshot_records = case include_snapshot {
    True -> [
      workflow_interface_snapshot_record(
        root,
        run_id,
        "execplan",
        workflow,
        fingerprint,
        at_ms + 5,
      ),
    ]
    False -> []
  }
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      list.append(
        [
          record.with_id(
            "workflow-started-" <> run_id,
            at_ms,
            record.WorkflowRunStarted(
              run_id: run_id,
              workflow_id: "execplan",
              workflow_fingerprint: fingerprint,
              issue_id: issue.id,
              issue_identifier: issue.identifier,
              issue_fingerprint: tracker_issue.content_fingerprint(issue),
              observed_updated_at_ms: at_ms - 1,
              run_root: root <> "/runs/" <> run_id,
            ),
          ),
          ..snapshot_records
        ],
        [
          seeded_output_manifest_record(root, run_id),
          record.with_id(
            "workflow-finished-" <> run_id,
            at_ms + 10,
            record.WorkflowRunFinished(
              run_id: run_id,
              workflow_id: "execplan",
              issue_id: issue.id,
              outcome: "completed",
              token_total: 0,
              turns: 1,
            ),
          ),
        ],
      ),
      True,
    )
  Nil
}

fn seed_interrupted_publication_retry_run(
  root: String,
  issue: tracker_issue.Issue,
  run_id: String,
  at_ms: Int,
  include_output_manifest include_output_manifest: Bool,
) -> Nil {
  write_seed_artifact(
    root,
    run_output_ref(run_id),
    commit_stack_payload(run_id),
  )
  write_seed_artifact(root, run_bundle_ref(run_id), "bundle")
  write_publication_retained_workspace_manifest(root, run_id)
  let output_records = case include_output_manifest {
    True -> [seeded_output_manifest_record(root, run_id)]
    False -> []
  }
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      list.append(
        [
          record.with_id(
            "workflow-started-" <> run_id,
            at_ms,
            record.WorkflowRunStarted(
              run_id: run_id,
              workflow_id: "execplan",
              workflow_fingerprint: workflow_fingerprint_for_publication_root(
                root,
              ),
              issue_id: issue.id,
              issue_identifier: issue.identifier,
              issue_fingerprint: tracker_issue.content_fingerprint(issue),
              observed_updated_at_ms: at_ms - 1,
              run_root: root <> "/runs/" <> run_id,
            ),
          ),
          ..output_records
        ],
        [
          record.with_id(
            "workflow-interrupted-" <> run_id,
            at_ms + 10,
            record.WorkflowRunInterrupted(
              run_id: run_id,
              workflow_id: "execplan",
              issue_id: issue.id,
              reason: "operator_stop",
            ),
          ),
          interrupted_publication_failed_attempt_record(
            root,
            issue,
            run_id,
            at_ms + 20,
          ),
        ],
      ),
      True,
    )
  Nil
}

fn seed_failed_publication_retry_run(
  root: String,
  issue: tracker_issue.Issue,
  run_id: String,
  at_ms: Int,
  include_output_manifest include_output_manifest: Bool,
) -> Nil {
  write_seed_artifact(
    root,
    run_output_ref(run_id),
    commit_stack_payload(run_id),
  )
  write_seed_artifact(root, run_bundle_ref(run_id), "bundle")
  write_publication_retained_workspace_manifest(root, run_id)
  let output_manifest = seeded_output_manifest(root, run_id)
  let config_path = publication_config_path(root)
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, workflow)) =
    runtime_bundle.workflow_by_id(bundle, "execplan")
  let assert Ok(body_templates) =
    artifact_publication_recording.load_body_templates(
      workflow_dag.publication_routes(workflow),
      bundle.orchestrator.artifact_repositories,
      bundle.orchestrator.config_dir,
      runtime_bundle.workflow_bundle_dir(bundle, workflow_dag.id(workflow)),
    )
  let assert Ok(fingerprint) =
    workflow_fingerprint_module.fingerprint_for_execution(
      workflow,
      bundle.orchestrator,
    )
  let assert [route] = workflow_dag.publication_routes(workflow)
  let assert Ok(planned) =
    artifact_publication_planner.plan_publication(
      output_manifest,
      bundle.orchestrator.artifact_repositories,
      route,
      artifact_store.new(root),
      artifact_publication_planner.PublicationWork(
        kind: artifact_publication_planner.TaskWork,
        id: issue.id,
        identifier: issue.identifier,
        slug: issue.identifier,
        title: None,
        url: None,
      ),
      run_id,
      body_templates,
    )
  let failed_ref =
    "runs/" <> run_id <> "/publications/execplan_review_doc/failed-1.json"
  let failed_manifest =
    artifact_publication_manifest.failed_from_planned_manifest(
      planned,
      "failed-1",
      at_ms + 20,
      True,
      Some(planned.branch),
      None,
      None,
      [],
      [],
      artifact_publication_manifest.PublicationErrorInfo(
        code: "git_push_failed",
        message: "previous push failed",
      ),
    )
  let #(failed_sha, failed_bytes) =
    write_publication_manifest(root, failed_ref, failed_manifest)
  let output_records = case include_output_manifest {
    True -> [seeded_output_manifest_record(root, run_id)]
    False -> []
  }
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      list.append(
        [
          record.with_id(
            "workflow-started-" <> run_id,
            at_ms,
            record.WorkflowRunStarted(
              run_id: run_id,
              workflow_id: "execplan",
              workflow_fingerprint: fingerprint,
              issue_id: issue.id,
              issue_identifier: issue.identifier,
              issue_fingerprint: tracker_issue.content_fingerprint(issue),
              observed_updated_at_ms: at_ms - 1,
              run_root: root <> "/runs/" <> run_id,
            ),
          ),
          ..output_records
        ],
        [
          record.with_id(
            "workflow-finished-" <> run_id,
            at_ms + 10,
            record.WorkflowRunFinished(
              run_id: run_id,
              workflow_id: "execplan",
              issue_id: issue.id,
              outcome: "completed",
              token_total: 0,
              turns: 1,
            ),
          ),
          record.with_id(
            "publication-failed-" <> run_id,
            at_ms + 20,
            record.PublicationAttemptRecorded(
              run_id: run_id,
              workflow_id: "execplan",
              publication_id: "execplan_review_doc",
              series_id: planned.series_id,
              attempt_id: "failed-1",
              status: "failed",
              required: True,
              retryable: True,
              retry_execution_available: True,
              version_id: None,
              manifest_ref: Some(failed_ref),
              manifest_sha256: Some(failed_sha),
              manifest_bytes: Some(failed_bytes),
              error_code: Some("git_push_failed"),
              error_message: Some("previous push failed"),
            ),
          ),
        ],
      ),
      True,
    )
  Nil
}

fn workflow_fingerprint_for_publication_root(root: String) -> String {
  let config_path = publication_config_path(root)
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, workflow)) =
    runtime_bundle.workflow_by_id(bundle, "execplan")
  let assert Ok(fingerprint) =
    workflow_fingerprint_module.fingerprint_for_execution(
      workflow,
      bundle.orchestrator,
    )
  fingerprint
}

fn seed_existing_superseding_run(
  root: String,
  issue: tracker_issue.Issue,
  superseded_run_id: String,
  superseding_run_id: String,
  at_ms: Int,
) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let fingerprint = workflow_fingerprint_for_publication_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "workflow-started-" <> superseding_run_id,
          at_ms,
          record.WorkflowRunStarted(
            run_id: superseding_run_id,
            workflow_id: "execplan",
            workflow_fingerprint: fingerprint,
            issue_id: issue.id,
            issue_identifier: issue.identifier,
            issue_fingerprint: tracker_issue.content_fingerprint(issue),
            observed_updated_at_ms: at_ms - 1,
            run_root: root <> "/runs/" <> superseding_run_id,
          ),
        ),
        record.with_id(
          "workflow-finished-" <> superseding_run_id,
          at_ms + 1,
          record.WorkflowRunFinished(
            run_id: superseding_run_id,
            workflow_id: "execplan",
            issue_id: issue.id,
            outcome: "superseding_run_reported",
            token_total: 0,
            turns: 1,
          ),
        ),
        record.with_id(
          "workflow-superseded-" <> superseded_run_id,
          at_ms + 10,
          record.WorkflowRunSuperseded(
            run_id: superseded_run_id,
            workflow_id: "execplan",
            issue_id: issue.id,
            superseded_by_run_id: superseding_run_id,
            reason: "dispatch_publication_recovery_superseded",
          ),
        ),
      ],
      True,
    )
  Nil
}

fn interrupted_publication_failed_attempt_record(
  root: String,
  issue: tracker_issue.Issue,
  run_id: String,
  at_ms: Int,
) -> record.LedgerRecord {
  let output_manifest = seeded_output_manifest(root, run_id)
  let config_path = publication_config_path(root)
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, workflow)) =
    runtime_bundle.workflow_by_id(bundle, "execplan")
  let assert Ok(body_templates) =
    artifact_publication_recording.load_body_templates(
      workflow_dag.publication_routes(workflow),
      bundle.orchestrator.artifact_repositories,
      bundle.orchestrator.config_dir,
      runtime_bundle.workflow_bundle_dir(bundle, workflow_dag.id(workflow)),
    )
  let assert [route] = workflow_dag.publication_routes(workflow)
  let assert Ok(planned) =
    artifact_publication_planner.plan_publication(
      output_manifest,
      bundle.orchestrator.artifact_repositories,
      route,
      artifact_store.new(root),
      artifact_publication_planner.PublicationWork(
        kind: artifact_publication_planner.TaskWork,
        id: issue.id,
        identifier: issue.identifier,
        slug: issue.identifier,
        title: None,
        url: None,
      ),
      run_id,
      body_templates,
    )
  let failed_ref =
    "runs/" <> run_id <> "/publications/execplan_review_doc/failed-1.json"
  let failed_manifest =
    artifact_publication_manifest.failed_from_planned_manifest(
      planned,
      "failed-1",
      at_ms,
      True,
      Some(planned.branch),
      None,
      None,
      [],
      [],
      artifact_publication_manifest.PublicationErrorInfo(
        code: "git_push_failed",
        message: "previous push failed",
      ),
    )
  let #(failed_sha, failed_bytes) =
    write_publication_manifest(root, failed_ref, failed_manifest)
  record.with_id(
    "publication-failed-" <> run_id,
    at_ms,
    record.PublicationAttemptRecorded(
      run_id: run_id,
      workflow_id: "execplan",
      publication_id: "execplan_review_doc",
      series_id: planned.series_id,
      attempt_id: "failed-1",
      status: "failed",
      required: True,
      retryable: True,
      retry_execution_available: True,
      version_id: None,
      manifest_ref: Some(failed_ref),
      manifest_sha256: Some(failed_sha),
      manifest_bytes: Some(failed_bytes),
      error_code: Some("git_push_failed"),
      error_message: Some("previous push failed"),
    ),
  )
}

fn seed_non_retryable_failed_publication_attempt(
  root: String,
  run_id: String,
  at_ms at_ms: Int,
) -> Nil {
  append_latest_publication_attempt(
    root,
    run_id,
    at_ms,
    "publication-nonretryable-failed-",
    "failed-non-retryable-1",
    "failed",
    Some("publication_not_retryable"),
    Some("publication failure cannot be retried"),
  )
}

fn seed_non_retryable_unchanged_publication_attempt(
  root: String,
  run_id: String,
  at_ms at_ms: Int,
) -> Nil {
  append_latest_publication_attempt(
    root,
    run_id,
    at_ms,
    "publication-nonretryable-unchanged-",
    "unchanged-non-retryable-1",
    "unchanged",
    None,
    None,
  )
}

fn append_latest_publication_attempt(
  root: String,
  run_id: String,
  at_ms: Int,
  record_id_prefix: String,
  attempt_id: String,
  status: String,
  error_code: Option(String),
  error_message: Option(String),
) -> Nil {
  let assert [latest, ..] =
    projection.publication_attempts_for_run(
      load_projection_or_panic(root),
      run_id,
      "execplan_review_doc",
    )
    |> list.reverse
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          record_id_prefix <> run_id,
          at_ms,
          record.PublicationAttemptRecorded(
            run_id: run_id,
            workflow_id: latest.workflow_id,
            publication_id: latest.publication_id,
            series_id: latest.series_id,
            attempt_id: attempt_id,
            status: status,
            required: latest.required,
            retryable: False,
            retry_execution_available: False,
            version_id: latest.version_id,
            manifest_ref: None,
            manifest_sha256: None,
            manifest_bytes: None,
            error_code: error_code,
            error_message: error_message,
          ),
        ),
      ],
      True,
    )
  Nil
}

fn seed_recovered_published_publication_run(
  root: String,
  issue: tracker_issue.Issue,
  run_id: String,
  at_ms: Int,
) -> Nil {
  write_seed_artifact(
    root,
    run_output_ref(run_id),
    commit_stack_payload(run_id),
  )
  write_seed_artifact(root, run_bundle_ref(run_id), "bundle")
  write_publication_retained_workspace_manifest(root, run_id)
  let output_manifest = seeded_output_manifest(root, run_id)
  let config_path = publication_config_path(root)
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, workflow)) =
    runtime_bundle.workflow_by_id(bundle, "execplan")
  let assert Ok(body_templates) =
    artifact_publication_recording.load_body_templates(
      workflow_dag.publication_routes(workflow),
      bundle.orchestrator.artifact_repositories,
      bundle.orchestrator.config_dir,
      runtime_bundle.workflow_bundle_dir(bundle, workflow_dag.id(workflow)),
    )
  let assert Ok(fingerprint) =
    workflow_fingerprint_module.fingerprint_for_execution(
      workflow,
      bundle.orchestrator,
    )
  let assert [route] = workflow_dag.publication_routes(workflow)
  let assert Ok(planned) =
    artifact_publication_planner.plan_publication(
      output_manifest,
      bundle.orchestrator.artifact_repositories,
      route,
      artifact_store.new(root),
      artifact_publication_planner.PublicationWork(
        kind: artifact_publication_planner.TaskWork,
        id: issue.id,
        identifier: issue.identifier,
        slug: issue.identifier,
        title: None,
        url: None,
      ),
      run_id,
      body_templates,
    )
  let published_ref =
    "runs/" <> run_id <> "/publications/execplan_review_doc/published-1.json"
  let published_manifest =
    artifact_publication_manifest.published_manifest(
      planned,
      "published-1",
      at_ms + 50,
      retry_commit_stack_head_sha(),
      Some("https://example.test/pr/543"),
      [],
      [],
    )
  let #(published_sha, published_bytes) =
    write_publication_manifest(root, published_ref, published_manifest)
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "workflow-started-" <> run_id,
          at_ms,
          record.WorkflowRunStarted(
            run_id: run_id,
            workflow_id: "execplan",
            workflow_fingerprint: fingerprint,
            issue_id: issue.id,
            issue_identifier: issue.identifier,
            issue_fingerprint: tracker_issue.content_fingerprint(issue),
            observed_updated_at_ms: at_ms - 1,
            run_root: root <> "/runs/" <> run_id,
          ),
        ),
        record.with_id(
          "workflow-failed-before-recovery-" <> run_id,
          at_ms + 10,
          record.WorkflowRunFinished(
            run_id: run_id,
            workflow_id: "execplan",
            issue_id: issue.id,
            outcome: "failed_fatal",
            token_total: 0,
            turns: 1,
          ),
        ),
        record.with_id(
          "workflow-repair-requested-" <> run_id,
          at_ms + 20,
          record.WorkflowRepairRequested(
            run_id: run_id,
            workflow_id: "execplan",
            issue_id: issue.id,
            issue_identifier: issue.identifier,
            requested_target: run_id,
            requested_step_id: Some("final_validate"),
            selected_step_id: "final_validate",
            failed_attempt_index: 2,
            next_attempt_index: 3,
            reason: "retry-step",
          ),
        ),
        record.with_id(
          "workflow-recovered-started-" <> run_id,
          at_ms + 30,
          record.WorkflowRunStarted(
            run_id: run_id,
            workflow_id: "execplan",
            workflow_fingerprint: fingerprint,
            issue_id: issue.id,
            issue_identifier: issue.identifier,
            issue_fingerprint: tracker_issue.content_fingerprint(issue),
            observed_updated_at_ms: at_ms - 1,
            run_root: root <> "/runs/" <> run_id,
          ),
        ),
        seeded_output_manifest_record(root, run_id),
        record.with_id(
          "workflow-recovered-finished-" <> run_id,
          at_ms + 40,
          record.WorkflowRunFinished(
            run_id: run_id,
            workflow_id: "execplan",
            issue_id: issue.id,
            outcome: "succeeded_after_recovery",
            token_total: 0,
            turns: 1,
          ),
        ),
        record.with_id(
          "publication-published-" <> run_id,
          at_ms + 50,
          record.PublicationAttemptRecorded(
            run_id: run_id,
            workflow_id: "execplan",
            publication_id: "execplan_review_doc",
            series_id: planned.series_id,
            attempt_id: "published-1",
            status: "published",
            required: True,
            retryable: False,
            retry_execution_available: True,
            version_id: Some(planned.version_id),
            manifest_ref: Some(published_ref),
            manifest_sha256: Some(published_sha),
            manifest_bytes: Some(published_bytes),
            error_code: None,
            error_message: None,
          ),
        ),
      ],
      True,
    )
  Nil
}

fn publication_config_path(root: String) -> String {
  let assert Ok(base) = path.dirname(root)
  base <> "/scherzo.yaml"
}

fn run_output_ref(run_id: String) -> String {
  "runs/" <> run_id <> "/outputs/commit-stack.json"
}

fn run_bundle_ref(run_id: String) -> String {
  "runs/" <> run_id <> "/outputs/commit-stack.bundle"
}

fn retry_commit_stack_base_sha() -> String {
  "1111111111111111111111111111111111111111"
}

fn retry_commit_stack_head_sha() -> String {
  "2222222222222222222222222222222222222222"
}

fn retry_commit_stack_tree_sha() -> String {
  "3333333333333333333333333333333333333333"
}

fn commit_stack_payload(run_id: String) -> String {
  "{\"artifact_type\":\"scherzo.git_commit_stack.v1\",\"repository\":\"scherzo-systems/scherzo\",\"base\":{\"ref\":\"main\",\"sha\":\""
  <> retry_commit_stack_base_sha()
  <> "\"},\"head\":{\"sha\":\""
  <> retry_commit_stack_head_sha()
  <> "\",\"tree\":\""
  <> retry_commit_stack_tree_sha()
  <> "\"},\"carrier\":{\"ref\":\""
  <> run_bundle_ref(run_id)
  <> "\",\"sha256\":\""
  <> hash.sha256_hex("bundle")
  <> "\",\"bytes\":6,\"media_type\":\"application/vnd.git.bundle\"}}"
}

fn commit_stack_driver_success_json(issue_identifier: String) -> String {
  "{\"version\":1,\"status\":\"published\",\"branch\":\"scherzo/execplan/"
  <> issue_identifier
  <> "\",\"base_ref\":\"main\",\"base_revision\":\""
  <> retry_commit_stack_base_sha()
  <> "\",\"head_revision\":\""
  <> retry_commit_stack_head_sha()
  <> "\",\"created\":false,\"updated\":true,\"url\":\"https://example.test/pr/42\",\"change_id\":\"42\"}"
}

fn seeded_output_manifest(
  root: String,
  run_id: String,
) -> workflow_contract_manifest.ContractOutputManifest {
  let body = commit_stack_payload(run_id)
  let written =
    workflow_contract_manifest.ArtifactWritten(
      ref: run_output_ref(run_id),
      sha256: hash.sha256_hex(body),
      bytes: bit_array.byte_size(bit_array.from_string(body)),
    )
  workflow_contract_manifest.ContractOutputManifest(
    run_id: run_id,
    workflow_id: "execplan",
    workflow_fingerprint: workflow_fingerprint_for_publication_root(root),
    outputs: [
      workflow_contract_manifest.NamedManifestValue(
        name: "commit_stack",
        value: workflow_contract_manifest.present_run_artifact(
          workflow_contract.CommitStack,
          written,
          "application/vnd.scherzo.git-commit-stack+json",
          None,
        ),
      ),
    ],
    diagnostics: [],
  )
}

fn write_publication_retained_workspace_manifest(
  root: String,
  run_id: String,
) -> Nil {
  let run_root = root <> "/runs/" <> run_id
  let workspace = run_root <> "/workspaces/main"
  let assert Ok(Nil) = simplifile.create_directory_all(workspace)
  let assert Ok(Nil) = simplifile.create_directory_all(run_root <> "/.scherzo")
  let assert Ok(Nil) =
    simplifile.write(
      workspace_manifest.manifest_path(run_root),
      workspace_manifest.encode_manifest(
        [
          workspace_manifest.Entry(
            run_id: run_id,
            workflow_id: "execplan",
            step_id: "materialize",
            attempt_index: 1,
            workspace_name: "main",
            relative_path: "workspaces/main",
            workspace_profile: "retained",
            driver_command: "retained-driver",
            driver_capabilities: ["publish-commit-stack"],
            source: workspace.FreshWorkspace,
            state: workspace_manifest.Ready,
          ),
        ],
        run_id,
        "execplan",
      ),
    )
  Nil
}

fn workflow_interface_snapshot_record(
  root: String,
  run_id: String,
  workflow_id: String,
  workflow: workflow_dag.WorkflowDag,
  fingerprint: String,
  at_ms: Int,
) -> record.LedgerRecord {
  let ref = artifact_store.workflow_interface_snapshot_ref(run_id)
  let contents =
    workflow_interface_snapshot.from_dag(workflow, fingerprint)
    |> workflow_interface_snapshot.to_string
  write_seed_artifact(root, ref, contents)
  record.with_id(
    "workflow-interface-snapshot-" <> run_id,
    at_ms,
    record.WorkflowInterfaceSnapshotRecorded(
      run_id: run_id,
      workflow_id: workflow_id,
      workflow_fingerprint: fingerprint,
      artifact_ref: ref,
      artifact_sha256: hash.sha256_hex(contents),
      artifact_bytes: bit_array.byte_size(bit_array.from_string(contents)),
    ),
  )
}

fn seeded_output_manifest_record(
  root: String,
  run_id: String,
) -> record.LedgerRecord {
  let payload =
    seeded_output_manifest(root, run_id)
    |> workflow_contract_manifest.output_manifest_to_string
  let ref = "runs/" <> run_id <> "/contract/outputs.json"
  write_seed_artifact(root, ref, payload)
  record.with_id(
    "workflow-outputs-recorded-" <> run_id,
    1015,
    record.WorkflowRunOutputsRecorded(
      run_id: run_id,
      workflow_id: "execplan",
      workflow_fingerprint: "wf-1",
      artifact_ref: ref,
      artifact_sha256: hash.sha256_hex(payload),
      artifact_bytes: bit_array.byte_size(bit_array.from_string(payload)),
    ),
  )
}

fn write_publication_manifest(
  root: String,
  ref: String,
  manifest: artifact_publication_manifest.PublicationManifest,
) -> #(String, Int) {
  let payload = artifact_publication_manifest.to_string(manifest)
  write_seed_artifact(root, ref, payload)
  #(
    hash.sha256_hex(payload),
    bit_array.byte_size(bit_array.from_string(payload)),
  )
}

fn write_seed_artifact(root: String, ref: String, contents: String) -> Nil {
  let absolute = root <> "/.scherzo-state/artifacts/" <> ref
  let assert Ok(dir) = path.dirname(absolute)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  let assert Ok(Nil) = simplifile.write(absolute, contents)
  Nil
}

fn publication_attempt_count(
  root: String,
  run_id: String,
  publication_id: String,
) -> Int {
  projection.publication_attempts_for_run(
    load_projection_or_panic(root),
    run_id,
    publication_id,
  )
  |> list.length
}

fn blocking_publication_retry_runner(
  log_subject: process.Subject(String),
  publish_barrier: test_async.Barrier,
) -> command_runner.Runner {
  let base = publication_retry_runner()
  command_runner.Runner(run: fn(spec) {
    let command_runner.CommandSpec(args: args, ..) = spec
    case args {
      ["publish-commit-stack", ..] -> {
        process.send(log_subject, "publication_driver_started")
        test_async.block_until_released(publish_barrier)
        base.run(spec)
      }
      _ -> base.run(spec)
    }
  })
}

fn failing_publication_retry_runner() -> command_runner.Runner {
  command_runner.Runner(run: fn(_) {
    Error(command_runner.command_error("driver exploded"))
  })
}

fn publication_retry_runner() -> command_runner.Runner {
  command_runner.Runner(run: fn(spec) {
    let command_runner.CommandSpec(
      executable: executable,
      args: args,
      cwd: cwd,
      ..,
    ) = spec
    let _ = simplifile.create_directory_all(cwd)
    case executable, args {
      "git", ["clone", _, target] -> {
        let _ = simplifile.create_directory_all(target)
        Ok(command_runner.CommandOutput(0, "", ""))
      }
      "git", ["fetch", ..]
      | "git", ["checkout", ..]
      | "git", ["status", ..]
      | "git", ["add", ..]
      | "git", ["commit", ..]
      -> Ok(command_runner.CommandOutput(0, "", ""))
      "git", ["ls-remote", ..] -> Ok(command_runner.CommandOutput(2, "", ""))
      "git", ["rev-parse", "--verify", ..] ->
        Ok(command_runner.CommandOutput(1, "", ""))
      "git", ["diff", ..] -> Ok(command_runner.CommandOutput(1, "", ""))
      "git", ["rev-parse", "HEAD"] ->
        Ok(command_runner.CommandOutput(0, "deadbeef", ""))
      "git", ["push", ..] -> Ok(command_runner.CommandOutput(0, "", ""))
      "gh", ["pr", "list", ..] -> Ok(command_runner.CommandOutput(0, "[]", ""))
      "gh", ["pr", "create", ..] ->
        Ok(command_runner.CommandOutput(0, "https://example.test/pr/1", ""))
      _, ["publish-commit-stack", ..] ->
        Ok(command_runner.CommandOutput(
          0,
          commit_stack_driver_success_json("LIV-1059"),
          "",
        ))
      _, _ -> Error(command_runner.command_error("unexpected_command"))
    }
  })
}

fn append_auto_unpark_issue_change_parked_record(
  root: String,
  issue: tracker_issue.Issue,
  at_ms: Int,
) -> Nil {
  append_auto_unpark_issue_change_parked_record_with_reason(
    root,
    issue,
    "worker_failure",
    at_ms,
  )
}

fn append_auto_unpark_issue_change_parked_record_with_reason(
  root: String,
  issue: tracker_issue.Issue,
  reason: String,
  at_ms: Int,
) -> Nil {
  append_parked_record(
    root,
    issue,
    reason,
    "auto_unpark_on_issue_change",
    at_ms,
  )
}

fn append_explicit_parked_record(
  root: String,
  issue: tracker_issue.Issue,
  reason: String,
  at_ms: Int,
) -> Nil {
  append_parked_record(root, issue, reason, "explicit_unpark_only", at_ms)
}

fn append_parked_record(
  root: String,
  issue: tracker_issue.Issue,
  reason: String,
  release_policy: String,
  at_ms: Int,
) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "issue-parked-" <> int.to_string(at_ms) <> "-" <> reason,
          at_ms,
          record.IssueParkedV2(
            issue.id,
            issue.identifier,
            reason,
            release_policy,
            tracker_issue.content_fingerprint(issue),
            at_ms,
          ),
        ),
      ],
      True,
    )
  Nil
}

fn seed_interrupted_retry_step_run(
  root: String,
  issue: tracker_issue.Issue,
  include_parked parked: Bool,
) -> Nil {
  seed_interrupted_retry_step_run_with_claim_lifecycle_and_fingerprint(
    root,
    issue,
    include_parked: parked,
    include_claim_lifecycle: False,
    workflow_fingerprint: "",
    interruption_reason: "daemon_shutdown",
  )
}

fn seed_interrupted_retry_step_run_with_interruption_reason(
  root: String,
  issue: tracker_issue.Issue,
  reason: String,
) -> Nil {
  seed_interrupted_retry_step_run_with_claim_lifecycle_and_fingerprint(
    root,
    issue,
    include_parked: False,
    include_claim_lifecycle: False,
    workflow_fingerprint: "",
    interruption_reason: reason,
  )
}

fn seed_interrupted_retry_step_run_with_workflow_fingerprint(
  root: String,
  issue: tracker_issue.Issue,
  include_parked parked: Bool,
  workflow_fingerprint workflow_fingerprint: String,
) -> Nil {
  seed_interrupted_retry_step_run_with_claim_lifecycle_and_fingerprint(
    root,
    issue,
    include_parked: parked,
    include_claim_lifecycle: False,
    workflow_fingerprint: workflow_fingerprint,
    interruption_reason: "daemon_shutdown",
  )
}

fn seed_claim_handoff_interrupted_retry_step_run(
  root: String,
  issue: tracker_issue.Issue,
) -> Nil {
  seed_interrupted_retry_step_run_with_claim_lifecycle_and_fingerprint(
    root,
    issue,
    include_parked: False,
    include_claim_lifecycle: True,
    workflow_fingerprint: "",
    interruption_reason: "daemon_shutdown",
  )
}

fn seed_interrupted_retry_step_run_with_claim_lifecycle_and_fingerprint(
  root: String,
  issue: tracker_issue.Issue,
  include_parked parked: Bool,
  include_claim_lifecycle claim_lifecycle: Bool,
  workflow_fingerprint workflow_fingerprint: String,
  interruption_reason interruption_reason: String,
) -> Nil {
  let run_root = root <> "/implementation/" <> issue.identifier <> "/run-1"
  let seed_workspace = run_root <> "/workspaces/seed"
  let workflow_workspace = root <> "/" <> issue.identifier
  let assert Ok(Nil) = simplifile.create_directory_all(seed_workspace)
  let store = artifact_store.new(root)
  let artifact =
    step_artifact.from_command_result(
      "seed",
      0,
      "done",
      "",
      False,
      [],
      artifact_limits(),
    )
  let assert Ok(written) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      "implementation",
      "seed",
      1,
      artifact,
    )
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let claim_offset = case claim_lifecycle {
    True -> 3
    False -> 0
  }
  let terminal_offset = case claim_lifecycle {
    True -> 1
    False -> 0
  }
  let claim_records = case claim_lifecycle {
    True -> [
      record.with_id(
        "known-workspace",
        2,
        record.KnownWorkspace(issue.id, issue.identifier, workflow_workspace),
      ),
      record.with_id(
        "run-started",
        3,
        record.RunStarted(
          "run-1",
          issue.id,
          issue.identifier,
          workflow_workspace,
        ),
      ),
      record.with_id(
        "issue-counter",
        4,
        record.IssueCounterUpdated(issue.id, issue.identifier, 0, 1, 100, None),
      ),
    ]
    False -> []
  }
  let workflow_started =
    record.with_id(
      "workflow-started",
      1,
      record.WorkflowRunStartedWithTask(
        run_id: "run-1",
        workflow_id: "implementation",
        workflow_fingerprint: workflow_fingerprint,
        issue_id: issue.id,
        issue_identifier: issue.identifier,
        task_ref: record.linear_task_ref_fields(
          issue.id,
          Some(issue.identifier),
          None,
        ),
        issue_fingerprint: tracker_issue.content_fingerprint(issue),
        observed_updated_at_ms: 100,
        run_root: run_root,
      ),
    )
  let step_records = [
    record.with_id(
      "seed-prepared",
      2 + claim_offset,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        workspace_name: "seed",
        workspace_path: seed_workspace,
        run_root: run_root,
        source_workspace_name: None,
        source_workspace_path: None,
      ),
    ),
    record.with_id(
      "seed-started",
      3 + claim_offset,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        operator_session_id: "session-seed-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "seed-finished",
      4 + claim_offset,
      record.StepAttemptFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        outcome: "completed",
        artifact_ref: written.ref,
        artifact_sha256: written.sha256,
        workspace_name: "seed",
        workspace_path: seed_workspace,
        token_total: 0,
        turns: 0,
      ),
    ),
    record.with_id(
      "feedback-prepared",
      5 + claim_offset,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        workspace_name: "derived",
        workspace_path: run_root <> "/workspaces/derived",
        run_root: run_root,
        source_workspace_name: Some("seed"),
        source_workspace_path: Some(seed_workspace),
      ),
    ),
    record.with_id(
      "feedback-started",
      6 + claim_offset,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        operator_session_id: "session-feedback-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "feedback-interrupted",
      7 + claim_offset,
      record.StepAttemptInterrupted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        reason: interruption_reason,
      ),
    ),
    record.with_id(
      "workflow-interrupted",
      8 + claim_offset,
      record.WorkflowRunInterrupted(
        run_id: "run-1",
        workflow_id: "implementation",
        issue_id: issue.id,
        reason: interruption_reason,
      ),
    ),
  ]
  let terminal_records = case claim_lifecycle {
    True -> [
      record.with_id(
        "run-interrupted",
        9 + claim_offset,
        record.RunInterrupted("run-1", issue.id, interruption_reason),
      ),
    ]
    False -> []
  }
  let base_records =
    list.append(
      [workflow_started],
      list.append(claim_records, list.append(step_records, terminal_records)),
    )
  let records = case parked {
    True ->
      list.append(base_records, [
        record.with_id(
          "issue-parked",
          9 + claim_offset + terminal_offset,
          record.IssueParkedV2(
            issue.id,
            issue.identifier,
            "operator_hold",
            "explicit_unpark_only",
            tracker_issue.content_fingerprint(issue),
            101,
          ),
        ),
      ])
    False -> base_records
  }
  let assert Ok(Nil) = ledger.append_many(ledger_path, records, True)
  Nil
}

fn seed_interrupted_retry_step_run_missing_provenance(
  root: String,
  issue: tracker_issue.Issue,
) -> Nil {
  let run_root = root <> "/implementation/" <> issue.identifier <> "/run-1"
  let seed_workspace = run_root <> "/workspaces/seed"
  let assert Ok(Nil) = simplifile.create_directory_all(seed_workspace)
  let store = artifact_store.new(root)
  let artifact =
    step_artifact.from_command_result(
      "seed",
      0,
      "done",
      "",
      False,
      [],
      artifact_limits(),
    )
  let assert Ok(written) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      "implementation",
      "seed",
      1,
      artifact,
    )
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let records = [
    record.with_id(
      "workflow-inputs",
      1,
      record.WorkflowRunInputsRecorded(
        run_id: "run-1",
        workflow_id: "implementation",
        workflow_fingerprint: "",
        artifact_ref: "runs/run-1/inputs.json",
        artifact_sha256: "sha-inputs",
        artifact_bytes: 10,
      ),
    ),
    record.with_id(
      "seed-prepared",
      2,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        workspace_name: "seed",
        workspace_path: seed_workspace,
        run_root: run_root,
        source_workspace_name: None,
        source_workspace_path: None,
      ),
    ),
    record.with_id(
      "seed-started",
      3,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        operator_session_id: "session-seed-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "seed-finished",
      4,
      record.StepAttemptFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        outcome: "completed",
        artifact_ref: written.ref,
        artifact_sha256: written.sha256,
        workspace_name: "seed",
        workspace_path: seed_workspace,
        token_total: 0,
        turns: 0,
      ),
    ),
    record.with_id(
      "feedback-prepared",
      5,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        workspace_name: "derived",
        workspace_path: run_root <> "/workspaces/derived",
        run_root: run_root,
        source_workspace_name: Some("seed"),
        source_workspace_path: Some(seed_workspace),
      ),
    ),
    record.with_id(
      "feedback-started",
      6,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        operator_session_id: "session-feedback-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "feedback-interrupted",
      7,
      record.StepAttemptInterrupted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "apply_feedback",
        attempt_index: 1,
        reason: "daemon_shutdown",
      ),
    ),
    record.with_id(
      "workflow-interrupted",
      8,
      record.WorkflowRunInterrupted(
        run_id: "run-1",
        workflow_id: "implementation",
        issue_id: issue.id,
        reason: "daemon_shutdown",
      ),
    ),
  ]
  let assert Ok(Nil) = simplifile.create_directory_all(ledger_path.ledger_dir)
  let assert Ok(Nil) =
    simplifile.write(
      ledger_path.current_path,
      records
        |> list.map(record.to_string)
        |> string.join(with: "\n")
        |> fn(contents) { contents <> "\n" },
    )
  Nil
}

fn seed_interrupted_review_retry_step_run(
  root: String,
  issue: tracker_issue.Issue,
) -> Nil {
  let run_root = root <> "/implementation/" <> issue.identifier <> "/run-1"
  let seed_workspace = run_root <> "/workspaces/seed"
  let main_workspace = run_root <> "/workspaces/main"
  let assert Ok(Nil) = simplifile.create_directory_all(seed_workspace)
  let assert Ok(Nil) = simplifile.create_directory_all(main_workspace)
  let store = artifact_store.new(root)
  let artifact =
    step_artifact.from_command_result(
      "seed",
      0,
      "done",
      "",
      False,
      [],
      artifact_limits(),
    )
  let assert Ok(written) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      "implementation",
      "seed",
      1,
      artifact,
    )
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let records = [
    record.with_id(
      "workflow-started",
      1,
      record.WorkflowRunStartedWithTask(
        run_id: "run-1",
        workflow_id: "implementation",
        workflow_fingerprint: "",
        issue_id: issue.id,
        issue_identifier: issue.identifier,
        task_ref: record.linear_task_ref_fields(
          issue.id,
          Some(issue.identifier),
          None,
        ),
        issue_fingerprint: tracker_issue.content_fingerprint(issue),
        observed_updated_at_ms: 100,
        run_root: run_root,
      ),
    ),
    record.with_id(
      "seed-prepared",
      2,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        workspace_name: "seed",
        workspace_path: seed_workspace,
        run_root: run_root,
        source_workspace_name: None,
        source_workspace_path: None,
      ),
    ),
    record.with_id(
      "seed-started",
      3,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        operator_session_id: "session-seed-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "seed-finished",
      4,
      record.StepAttemptFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        outcome: "completed",
        artifact_ref: written.ref,
        artifact_sha256: written.sha256,
        workspace_name: "seed",
        workspace_path: seed_workspace,
        token_total: 0,
        turns: 0,
      ),
    ),
    record.with_id(
      "implement-prepared",
      5,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        attempt_index: 1,
        workspace_name: "main",
        workspace_path: main_workspace,
        run_root: run_root,
        source_workspace_name: Some("seed"),
        source_workspace_path: Some(seed_workspace),
      ),
    ),
    record.with_id(
      "implement-started",
      6,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        attempt_index: 1,
        operator_session_id: "session-implement-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "implement-interrupted",
      7,
      record.StepAttemptInterrupted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        attempt_index: 1,
        reason: "daemon_shutdown",
      ),
    ),
    record.with_id(
      "workflow-interrupted",
      8,
      record.WorkflowRunInterrupted(
        run_id: "run-1",
        workflow_id: "implementation",
        issue_id: issue.id,
        reason: "daemon_shutdown",
      ),
    ),
  ]
  let assert Ok(Nil) = ledger.append_many(ledger_path, records, True)
  Nil
}

fn seed_orphaned_review_children_run(
  root: String,
  issue: tracker_issue.Issue,
) -> Nil {
  let run_root = root <> "/implementation/" <> issue.identifier <> "/run-1"
  let seed_workspace = run_root <> "/workspaces/seed"
  let main_workspace = run_root <> "/workspaces/main"
  let code_review_workspace = run_root <> "/workspaces/code-review"
  let security_review_workspace = run_root <> "/workspaces/security-review"
  let assert Ok(Nil) = simplifile.create_directory_all(seed_workspace)
  let assert Ok(Nil) = simplifile.create_directory_all(main_workspace)
  let assert Ok(Nil) = simplifile.create_directory_all(code_review_workspace)
  let assert Ok(Nil) =
    simplifile.create_directory_all(security_review_workspace)
  let store = artifact_store.new(root)
  let seed_artifact =
    step_artifact.from_command_result(
      "seed",
      0,
      "done",
      "",
      False,
      [],
      artifact_limits(),
    )
  let assert Ok(seed_written) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      "implementation",
      "seed",
      1,
      seed_artifact,
    )
  let implement_artifact =
    step_artifact.from_command_result(
      "implement",
      0,
      "done",
      "",
      False,
      [],
      artifact_limits(),
    )
  let assert Ok(implement_written) =
    artifact_store.write_step_artifact(
      store,
      "run-1",
      "implementation",
      "implement",
      1,
      implement_artifact,
    )
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let records = [
    record.with_id(
      "workflow-started",
      1,
      record.WorkflowRunStartedWithTask(
        run_id: "run-1",
        workflow_id: "implementation",
        workflow_fingerprint: "",
        issue_id: issue.id,
        issue_identifier: issue.identifier,
        task_ref: record.linear_task_ref_fields(
          issue.id,
          Some(issue.identifier),
          None,
        ),
        issue_fingerprint: tracker_issue.content_fingerprint(issue),
        observed_updated_at_ms: 100,
        run_root: run_root,
      ),
    ),
    record.with_id(
      "seed-prepared",
      2,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        workspace_name: "seed",
        workspace_path: seed_workspace,
        run_root: run_root,
        source_workspace_name: None,
        source_workspace_path: None,
      ),
    ),
    record.with_id(
      "seed-started",
      3,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        operator_session_id: "session-seed-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "seed-finished",
      4,
      record.StepAttemptFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "seed",
        attempt_index: 1,
        outcome: "completed",
        artifact_ref: seed_written.ref,
        artifact_sha256: seed_written.sha256,
        workspace_name: "seed",
        workspace_path: seed_workspace,
        token_total: 0,
        turns: 0,
      ),
    ),
    record.with_id(
      "implement-prepared",
      5,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        attempt_index: 1,
        workspace_name: "main",
        workspace_path: main_workspace,
        run_root: run_root,
        source_workspace_name: Some("seed"),
        source_workspace_path: Some(seed_workspace),
      ),
    ),
    record.with_id(
      "implement-started",
      6,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        attempt_index: 1,
        operator_session_id: "session-implement-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "implement-finished",
      7,
      record.StepAttemptFinished(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "implement",
        attempt_index: 1,
        outcome: "completed",
        artifact_ref: implement_written.ref,
        artifact_sha256: implement_written.sha256,
        workspace_name: "main",
        workspace_path: main_workspace,
        token_total: 0,
        turns: 0,
      ),
    ),
    record.with_id(
      "code-review-prepared",
      8,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "code_review",
        attempt_index: 1,
        workspace_name: "code-review",
        workspace_path: code_review_workspace,
        run_root: run_root,
        source_workspace_name: Some("main"),
        source_workspace_path: Some(main_workspace),
      ),
    ),
    record.with_id(
      "code-review-started",
      9,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "code_review",
        attempt_index: 1,
        operator_session_id: "session-code-review-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "security-review-prepared",
      10,
      record.StepAttemptPrepared(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "security_review",
        attempt_index: 1,
        workspace_name: "security-review",
        workspace_path: security_review_workspace,
        run_root: run_root,
        source_workspace_name: Some("main"),
        source_workspace_path: Some(main_workspace),
      ),
    ),
    record.with_id(
      "security-review-started",
      11,
      record.StepAttemptStarted(
        run_id: "run-1",
        workflow_id: "implementation",
        step_id: "security_review",
        attempt_index: 1,
        operator_session_id: "session-security-review-1",
        external_session_ref: None,
        continuation_capable: False,
      ),
    ),
    record.with_id(
      "workflow-interrupted",
      12,
      record.WorkflowRunInterrupted(
        run_id: "run-1",
        workflow_id: "implementation",
        issue_id: issue.id,
        reason: "daemon_shutdown",
      ),
    ),
  ]
  let assert Ok(Nil) = ledger.append_many(ledger_path, records, True)
  Nil
}

fn artifact_limits() -> config_types.ArtifactLimits {
  config_types.ArtifactLimits(
    command_stream_max_chars: 1000,
    template_field_max_chars: 1000,
    workflow_summary_max_chars: 4000,
  )
}

fn assert_retry_step_queued(
  result: command.CommandResult,
  step_id: Option(String),
) -> String {
  assert command.status_reason(result.status) == None
  assert command.status_to_string(result.status) == "queued"
  let assert Some(operation_id) = result.operation_id
  assert string.starts_with(
    operation_id,
    "retry-step:run-1:" <> option.unwrap(step_id, "auto") <> ":",
  )
  assert result.message
    == Some("retry-step accepted; poll query operation-status for completion")
  operation_id
}

fn wait_for_operation_status(
  root: String,
  operation_id: String,
  expected_status: String,
  attempts: Int,
) -> Result(projection.ControlOperationStatus, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case
        projection.control_operation(
          load_projection_or_panic(root),
          operation_id,
        )
      {
        Ok(operation) ->
          case operation.status == expected_status {
            True -> Ok(operation)
            False -> {
              process.sleep(20)
              wait_for_operation_status(
                root,
                operation_id,
                expected_status,
                attempts - 1,
              )
            }
          }
        Error(Nil) -> {
          process.sleep(20)
          wait_for_operation_status(
            root,
            operation_id,
            expected_status,
            attempts - 1,
          )
        }
      }
  }
}

fn append_ledger_records(
  root: String,
  records: List(record.LedgerRecord),
) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = ledger.append_many(ledger_path, records, True)
  Nil
}

fn chmod_path(mode: String, path: String) -> Nil {
  let assert Ok(chmod) = port.start_argv("chmod", [mode, path], ".", [])
  let assert Ok(0) = port.await_exit(chmod, 1000)
  Nil
}

fn ledger_bodies(root: String) -> List(record.RecordBody) {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(read) = ledger.read_records(ledger_path)
  list.map(read.records, fn(ledger_record) { ledger_record.body })
}

fn ledger_has_issue_unparked(
  root: String,
  expected_issue_id: String,
  expected_reason: String,
) -> Bool {
  list.any(ledger_bodies(root), fn(body) {
    case body {
      record.IssueUnparked(issue_id, _, reason) ->
        issue_id == expected_issue_id && reason == expected_reason
      _ -> False
    }
  })
}

fn ledger_has_issue_counter_reset(
  root: String,
  expected_issue_id: String,
) -> Bool {
  list.any(ledger_bodies(root), fn(body) {
    case body {
      record.IssueCounterUpdated(issue_id, _, 0, 0, _, _) ->
        issue_id == expected_issue_id
      _ -> False
    }
  })
}

fn retained_workflow_diagnostic_reason(root: String, expected: String) -> Bool {
  list.any(ledger_bodies(root), fn(body) {
    case body {
      record.WorkflowRunDiagnostic(reason: reason, ..) -> reason == expected
      _ -> False
    }
  })
}

fn retained_workflow_interruption_reason(
  root: String,
  expected: String,
) -> Bool {
  list.any(ledger_bodies(root), fn(body) {
    case body {
      record.WorkflowRunInterrupted(reason: reason, ..) -> reason == expected
      _ -> False
    }
  })
}

fn contains_kind(root: String, expected: String) -> Bool {
  ledger_bodies(root)
  |> list.any(fn(body) { record.kind(body) == expected })
}

fn count_kind(root: String, expected: String) -> Int {
  ledger_bodies(root)
  |> list.filter(fn(body) { record.kind(body) == expected })
  |> list.length
}

fn contains_kind_sequence(root: String, expected: List(String)) -> Bool {
  contains_sequence(ledger_bodies(root) |> list.map(record.kind), expected)
}

fn contains_sequence(values: List(String), expected: List(String)) -> Bool {
  case expected {
    [] -> True
    _ ->
      case values {
        [] -> False
        [_first, ..rest] ->
          case list.take(values, list.length(expected)) == expected {
            True -> True
            False -> contains_sequence(rest, expected)
          }
      }
  }
}

fn worker_success(
  issue: tracker_issue.Issue,
  workspace_path: String,
) -> agent_types.WorkerSuccess {
  agent_types.WorkerSuccess(
    final_issue: Some(issue),
    final_classification: agent_types.FinalTerminal,
    workspace_path: workspace_path,
    tokens: session_tokens.zero_token_totals(),
    turns: 1,
    result: result_artifact.from_final_response(None, False, "none"),
  )
}

fn has_step_interrupted_reason(
  root: String,
  step_id: String,
  expected: String,
) -> Bool {
  count_step_interrupted_reason(root, step_id, expected) > 0
}

fn has_step_interrupted_attempt_reason(
  root: String,
  step_id: String,
  attempt_index: Int,
  expected: String,
) -> Bool {
  ledger_bodies(root)
  |> list.any(fn(body) {
    case body {
      record.StepAttemptInterrupted(
        step_id: body_step_id,
        attempt_index: body_attempt_index,
        reason: reason,
        ..,
      ) ->
        body_step_id == step_id
        && body_attempt_index == attempt_index
        && reason == expected
      _ -> False
    }
  })
}

fn has_step_finished_attempt(
  root: String,
  step_id: String,
  attempt_index: Int,
) -> Bool {
  ledger_bodies(root)
  |> list.any(fn(body) {
    case body {
      record.StepAttemptFinished(
        step_id: body_step_id,
        attempt_index: body_attempt_index,
        ..,
      ) -> body_step_id == step_id && body_attempt_index == attempt_index
      _ -> False
    }
  })
}

fn count_step_interrupted_reason(
  root: String,
  step_id: String,
  expected: String,
) -> Int {
  ledger_bodies(root)
  |> list.filter(fn(body) {
    case body {
      record.StepAttemptInterrupted(step_id: body_step_id, reason: reason, ..) ->
        body_step_id == step_id && reason == expected
      _ -> False
    }
  })
  |> list.length
}

fn recovery_append_state(
  _log_subject: process.Subject(String),
  root: String,
) -> String {
  case
    contains_kind_sequence(root, [
      "workflow_repair_requested",
      "step_attempt_superseded",
      "workflow_run_started",
      "known_workspace",
      "issue_counter_updated",
    ])
  {
    True -> "retry_step_ledger_ready"
    False -> "retry_step_ledger_missing"
  }
}

fn wait_for_parent_session(
  subject: process.Subject(hub.Message),
  issue_identifier: String,
  attempts: Int,
) -> Result(event.SessionSummary, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case hub.list_sessions(subject, 250) {
        Ok(sessions) -> {
          let parent =
            list.find(sessions, fn(summary) {
              summary.issue_identifier == issue_identifier
              && !string.starts_with(summary.session_id, "workflow-step-")
            })
          case parent {
            Ok(summary) -> Ok(summary)
            Error(Nil) -> {
              process.sleep(50)
              wait_for_parent_session(subject, issue_identifier, attempts - 1)
            }
          }
        }
        Error(_) -> {
          process.sleep(50)
          wait_for_parent_session(subject, issue_identifier, attempts - 1)
        }
      }
  }
}

fn wait_for_active_step_session(
  subject: process.Subject(hub.Message),
  run_id: String,
  step_id: String,
  attempt_index: Int,
  attempts: Int,
) -> Result(event.SessionSummary, Nil) {
  let expected_session_id = yaml_step_session.id(run_id, step_id, attempt_index)
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case hub.list_sessions(subject, 250) {
        Ok(sessions) -> {
          let session =
            list.find(sessions, fn(summary) {
              summary.status == event.Running
              && summary.session_id == expected_session_id
            })
          case session {
            Ok(summary) -> Ok(summary)
            Error(Nil) -> {
              process.sleep(50)
              wait_for_active_step_session(
                subject,
                run_id,
                step_id,
                attempt_index,
                attempts - 1,
              )
            }
          }
        }
        Error(_) -> {
          process.sleep(50)
          wait_for_active_step_session(
            subject,
            run_id,
            step_id,
            attempt_index,
            attempts - 1,
          )
        }
      }
  }
}

fn wait_for_step_session(
  subject: process.Subject(hub.Message),
  step_id: String,
  attempts: Int,
) -> Result(event.SessionSummary, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case hub.list_sessions(subject, 250) {
        Ok(sessions) -> {
          let session =
            list.find(sessions, fn(summary) {
              case summary.recovery {
                Some(recovery) -> recovery.workflow_step_id == Some(step_id)
                None -> False
              }
            })
          case session {
            Ok(summary) -> Ok(summary)
            Error(Nil) -> {
              process.sleep(50)
              wait_for_step_session(subject, step_id, attempts - 1)
            }
          }
        }
        Error(_) -> {
          process.sleep(50)
          wait_for_step_session(subject, step_id, attempts - 1)
        }
      }
  }
}

fn assert_active_child_has_no_orphan_cleanup_recovery(
  summary: event.SessionSummary,
) -> Nil {
  case summary.recovery {
    None -> Nil
    Some(recovery) -> {
      assert recovery.status != event.Cleanup
      assert recovery.source != "workflow.yaml_step_orphan_cleanup"
    }
  }
}

fn assert_child_orphan_recovery(
  summary: event.SessionSummary,
  step_id: String,
) -> Nil {
  assert_child_orphan_recovery_for_attempt(summary, step_id, 1, "Todo")
}

fn assert_child_orphan_recovery_for_attempt(
  summary: event.SessionSummary,
  step_id: String,
  attempt_index: Int,
  expected_issue_state: String,
) -> Nil {
  let assert Some(recovery) = summary.recovery
  assert recovery.status == event.Cleanup
  assert recovery.source == "workflow.yaml_step_orphan_cleanup"
  assert recovery.workflow_run_id == Some("run-1")
  assert recovery.workflow_step_id == Some(step_id)
  assert recovery.workflow_attempt_index == Some(attempt_index)
  assert recovery.parent_session_id == Some("run-1")
  assert recovery.orphan_status == Some("orphaned_parent_stopped")
  assert recovery.issue_state == Some(expected_issue_state)
  assert recovery.recommended_action == Some("cleanup_orphan_steps")
}

fn wait_for_all_logs(
  subject: process.Subject(String),
  expected: List(String),
  attempts: Int,
) -> Bool {
  wait_for_all_logs_loop(subject, expected, [], attempts)
}

fn wait_for_all_logs_loop(
  subject: process.Subject(String),
  expected: List(String),
  seen: List(String),
  attempts: Int,
) -> Bool {
  case list.all(expected, fn(entry) { list.contains(seen, entry) }) {
    True -> True
    False ->
      case attempts <= 0 {
        True -> False
        False ->
          case process.receive(subject, within: 100) {
            Ok(actual) -> {
              let next_seen = case list.contains(expected, actual) {
                True -> [actual, ..seen]
                False -> seen
              }
              wait_for_all_logs_loop(subject, expected, next_seen, attempts)
            }
            Error(_) ->
              wait_for_all_logs_loop(subject, expected, seen, attempts - 1)
          }
      }
  }
}

fn wait_for_log(
  subject: process.Subject(String),
  expected: String,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case process.receive(subject, within: 100) {
        Ok(actual) ->
          case actual == expected {
            True -> True
            False -> wait_for_log(subject, expected, attempts - 1)
          }
        Error(_) -> wait_for_log(subject, expected, attempts - 1)
      }
  }
}
