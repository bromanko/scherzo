import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/control/protocol
import scherzo/error
import scherzo/handoff
import scherzo/hash
import scherzo/orchestrator/daemon
import scherzo/orchestrator/yaml_step_session
import scherzo/path
import scherzo/result_artifact
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/reason as session_reason
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/record
import scherzo/step_artifact
import scherzo/task
import scherzo/tracker
import scherzo/tracker/adapter
import scherzo/tracker/adapter_legacy
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_run
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
  assert wait_for_log(log_subject, "dispatch_started", 20)

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
  assert command.status_reason(result.status) == Some("issue_already_active")

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
      fn(issue, context, _effective) {
        process.send(log_subject, "recovered_worker_started:" <> issue.id)
        assert context.step_id == "apply_feedback"
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

  assert command.status_to_string(result.status) == "applied"
  assert command.status_reason(result.status) == None
  assert contains_kind_sequence(root, [
    "workflow_repair_requested",
    "step_attempt_superseded",
    "workflow_run_started",
  ])
  assert wait_for_log(log_subject, "recovered_worker_started:issue-1", 20)

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
    20,
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
    20,
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
  retries:
    attempts: 3
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
  retries:
    attempts: 3
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
  retries:
    attempts: 3
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
  daemon.RuntimeDependencies(
    ..daemon.default_dependencies(),
    make_tracker_adapter: fn(_) {
      let legacy =
        adapter_legacy.adapter_from_legacy_client(tracker_client, "linear")
      adapter.TrackerAdapter(
        ..legacy,
        handoff: Some(test_handoff_capability(disabled_handoff())),
      )
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
    start_event_hub: fn() { Ok(hub_subject) },
    make_control_token: fn() { Ok("test-token") },
    start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
    stop_control_server: fn(_) { Nil },
  )
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
  let assert Some(recovery) = summary.recovery
  assert recovery.status == event.Cleanup
  assert recovery.source == "workflow.yaml_step_orphan_cleanup"
  assert recovery.workflow_run_id == Some("run-1")
  assert recovery.workflow_step_id == Some(step_id)
  assert recovery.workflow_attempt_index == Some(1)
  assert recovery.parent_session_id == Some("run-1")
  assert recovery.orphan_status == Some("orphaned_parent_stopped")
  assert recovery.issue_state == Some("Todo")
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
            Ok(actual) ->
              wait_for_all_logs_loop(
                subject,
                expected,
                [actual, ..seen],
                attempts - 1,
              )
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
