import gleam/bit_array
import gleam/erlang/process
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
import scherzo/error
import scherzo/handoff
import scherzo/hash
import scherzo/orchestrator/daemon
import scherzo/orchestrator/dispatch_recovery
import scherzo/orchestrator/startup_recovery
import scherzo/orchestrator/yaml_step_session
import scherzo/path
import scherzo/result_artifact
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
import scherzo/workflow_fingerprint as workflow_fingerprint_module
import scherzo/workflow_run
import scherzo/workspace_manifest
import simplifile
import support/test_helpers
import test_async

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

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "agent_run:issue-1", 20)

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

pub fn retry_step_appends_repair_records_before_spawning_recovered_worker_test() {
  let dir = "test/tmp/daemon-retry-step-accepted"
  let issue = issue("issue-1", "LIV-509", "Todo")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
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
  assert command.status_to_string(result.status) == "applied"
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

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )

  assert command.status_to_string(result.status) == "applied"
  assert contains_kind_sequence(root, [
    "workflow_run_started",
    "known_workspace",
    "run_started",
    "issue_counter_updated",
  ])
  assert contains_kind_sequence(root, [
    "workflow_run_interrupted",
    "run_interrupted",
    "workflow_repair_requested",
    "step_attempt_superseded",
    "workflow_run_started",
  ])
  assert wait_for_log(log_subject, "recovered_worker_started:issue-1", 20)

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

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )

  assert command.status_to_string(result.status) == "applied"
  assert result.message
    == Some(
      "provenance_repaired; retrying run run-1 step apply_feedback at attempt 2",
    )
  assert contains_kind_sequence(root, [
    "workflow_run_provenance_repaired",
    "workflow_repair_requested",
    "step_attempt_superseded",
    "workflow_run_started",
  ])
  assert wait_for_log(log_subject, "recovered_worker_started:issue-1", 20)

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_artifact_recovery_failure_returns_detail_and_retains_diagnostic_test() {
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

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(
        command.RetryWorkflowStepRunId("run-1"),
        Some("apply_feedback"),
      ),
      1000,
    )

  let detail =
    "artifact_recovery_failed: step_id=seed artifact_ref="
    <> artifact_ref
    <> " reason=sha_mismatch expected_sha256="
    <> expected_sha256
    <> " current_sha256="
    <> current_sha256
  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status)
    == Some("artifact_recovery_failed")
  assert result.message
    == Some("retry-step repair was rejected by recovery validation: " <> detail)
  assert retained_workflow_diagnostic_reason(root, detail)
  assert !retained_workflow_interruption_reason(root, detail)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_does_not_append_provenance_repair_when_finalization_rejects_test() {
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
    == Some("artifact_recovery_failed")
  assert !contains_kind(root, "workflow_run_provenance_repaired")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_step_rejects_non_active_non_terminal_issue_state_for_retained_run_test() {
  let dir = "test/tmp/daemon-retry-step-non-active"
  let issue = issue("issue-1", "LIV-510", "Triage")
  let #(workflow_path, root) = write_retry_step_workflow(dir)
  seed_interrupted_retry_step_run(root, issue, include_parked: False)
  let before = ledger_bodies(root)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_with_candidate(issue),
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

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "candidates_fetched", 20)
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

  assert command.status_to_string(result.status) == "rejected"
  assert command.status_reason(result.status)
    == Some("issue_state_drift:non_active_state")
  assert result.message
    == Some(
      "run run-1 for issue LIV-510 is currently in non-active state Triage; move the issue to a configured active state before retry-step",
    )
  assert ledger_bodies(root) == before

  let assert Ok(run_result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(command.RetryWorkflowStepRunId("run-1"), None),
      1000,
    )
  assert command.status_to_string(run_result.status) == "rejected"
  assert command.status_reason(run_result.status)
    == Some("issue_state_drift:non_active_state")
  assert run_result.message == result.message
  assert ledger_bodies(root) == before

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

  let assert Ok(retry_result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(command.RetryWorkflowStepRunId("run-1"), None),
      1000,
    )
  assert command.status_to_string(retry_result.status) == "applied"
  assert wait_for_all_logs(
    log_subject,
    [
      "review_started:code_review",
      "review_started:security_review",
    ],
    100,
  )

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

  let assert Ok(retry_result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(command.RetryWorkflowStepRunId("run-1"), None),
      1000,
    )
  assert command.status_to_string(retry_result.status) == "applied"
  assert wait_for_all_logs(
    log_subject,
    [
      "shutdown_review_started:code_review",
      "shutdown_review_started:security_review",
    ],
    100,
  )

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

  let assert Ok(retry_result) =
    daemon.apply_operator_command(
      active_started.data,
      command.RetryWorkflowStep(command.RetryWorkflowStepRunId("run-1"), None),
      1000,
    )
  assert command.status_to_string(retry_result.status) == "applied"
  assert wait_for_all_logs(
    log_subject,
    [
      "active_review_started:code_review",
      "active_review_started:security_review",
    ],
    100,
  )
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

  let assert Ok(retry_result) =
    daemon.apply_operator_command(
      active_started.data,
      command.RetryWorkflowStep(command.RetryWorkflowStepRunId("run-1"), None),
      1000,
    )
  assert command.status_to_string(retry_result.status) == "applied"
  assert wait_for_log(log_subject, "active_command_started:apply_feedback", 20)

  let assert Ok(active_command_session) =
    wait_for_active_step_session(hub_subject, "run-1", "apply_feedback", 2, 20)
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

  let assert Ok(retry_result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryWorkflowStep(command.RetryWorkflowStepRunId("run-1"), None),
      1000,
    )
  assert command.status_to_string(retry_result.status) == "applied"
  assert wait_for_log(log_subject, "active_command_started:apply_feedback", 20)

  let assert Ok(active_command_session) =
    wait_for_active_step_session(hub_subject, "run-1", "apply_feedback", 2, 20)
  assert_active_child_has_no_orphan_cleanup_recovery(active_command_session)

  set_issue_sequence(issue_subject, non_active_issue)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "worker_stop_requested", 20)

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
  assert ledger_bodies(root) == before

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
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

pub fn dispatch_recovery_classifier_rejects_publication_issue_drift_test() {
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

  let assert dispatch_recovery.RejectRecovery(reason, _) =
    dispatch_recovery.classify(
      load_projection_or_panic(root),
      changed_issue,
      observation_for(workflow_path, changed_issue),
    )
  assert reason == "publication_recovery_issue_drift"
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

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_log(log_subject, "state_transition:Triage", 100)
  assert wait_for_log(log_subject, "dispatch_recovery_rejected", 100)
  assert !wait_for_log(log_subject, "agent_run:issue-1", 5)
  assert contains_kind_sequence(root, ["issue_parked_v2"])

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
    make_tracker_adapter: make_tracker_adapter,
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
      "#!/bin/sh\nif [ \"$1\" = describe ] && [ \"$2\" = --json ]; then\n  printf '%s\\n' '{\"version\":1,\"capabilities\":[\"publish-commit-stack\"]}'\n  exit 0\nfi\nexit 1\n",
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
  let output_manifest = seeded_output_manifest(run_id)
  let config_path = publication_config_path(root)
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, workflow)) =
    runtime_bundle.workflow_by_id(bundle, "execplan")
  let assert Ok(body_templates) =
    artifact_publication_recording.load_body_templates(
      workflow.publication_routes,
      bundle.orchestrator.artifact_repositories,
      bundle.orchestrator.config_dir,
      runtime_bundle.workflow_bundle_dir(bundle, workflow.id),
    )
  let assert Ok(fingerprint) =
    workflow_fingerprint_module.fingerprint_for_execution(
      workflow,
      bundle.orchestrator,
    )
  let assert [route] = workflow.publication_routes
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
  let output_manifest = seeded_output_manifest(run_id)
  let config_path = publication_config_path(root)
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, workflow)) =
    runtime_bundle.workflow_by_id(bundle, "execplan")
  let assert Ok(body_templates) =
    artifact_publication_recording.load_body_templates(
      workflow.publication_routes,
      bundle.orchestrator.artifact_repositories,
      bundle.orchestrator.config_dir,
      runtime_bundle.workflow_bundle_dir(bundle, workflow.id),
    )
  let assert Ok(fingerprint) =
    workflow_fingerprint_module.fingerprint_for_execution(
      workflow,
      bundle.orchestrator,
    )
  let assert [route] = workflow.publication_routes
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
    workflow_fingerprint: "wf-1",
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
            source_workspace_name: None,
            source_workspace_relative_path: None,
            state: workspace_manifest.Ready,
          ),
        ],
        run_id,
        "execplan",
      ),
    )
  Nil
}

fn seeded_output_manifest_record(
  root: String,
  run_id: String,
) -> record.LedgerRecord {
  let payload =
    seeded_output_manifest(run_id)
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
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "issue-auto-parked",
          at_ms,
          record.IssueParkedV2(
            issue.id,
            issue.identifier,
            "worker_failure",
            "auto_unpark_on_issue_change",
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
  seed_interrupted_retry_step_run_with_claim_lifecycle(
    root,
    issue,
    include_parked: parked,
    include_claim_lifecycle: False,
  )
}

fn seed_claim_handoff_interrupted_retry_step_run(
  root: String,
  issue: tracker_issue.Issue,
) -> Nil {
  seed_interrupted_retry_step_run_with_claim_lifecycle(
    root,
    issue,
    include_parked: False,
    include_claim_lifecycle: True,
  )
}

fn seed_interrupted_retry_step_run_with_claim_lifecycle(
  root: String,
  issue: tracker_issue.Issue,
  include_parked parked: Bool,
  include_claim_lifecycle claim_lifecycle: Bool,
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
        reason: "daemon_shutdown",
      ),
    ),
    record.with_id(
      "workflow-interrupted",
      8 + claim_offset,
      record.WorkflowRunInterrupted(
        run_id: "run-1",
        workflow_id: "implementation",
        issue_id: issue.id,
        reason: "daemon_shutdown",
      ),
    ),
  ]
  let terminal_records = case claim_lifecycle {
    True -> [
      record.with_id(
        "run-interrupted",
        9 + claim_offset,
        record.RunInterrupted("run-1", issue.id, "daemon_shutdown"),
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

fn ledger_bodies(root: String) -> List(record.RecordBody) {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(read) = ledger.read_records(ledger_path)
  list.map(read.records, fn(ledger_record) { ledger_record.body })
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
