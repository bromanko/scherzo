import birl
import gleam/list
import gleam/option.{None, Some}
import scherzo/agent/types as agent_types
import scherzo/config
import scherzo/error
import scherzo/orchestrator/effect_completion_handler
import scherzo/orchestrator/effect_runner
import scherzo/result_artifact
import scherzo/session/tokens as session_tokens
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_policy

pub fn handle_completed_dispatches_finished_variants_test() {
  let publication = scheduled_failure_publication()
  let receipt = scheduled_failure_receipt()
  let state = new_state()
  let state =
    apply_completion(
      state,
      effect_runner.Finished(
        1,
        effect_runner.CandidateFetchFinished(1, Ok([issue("issue-1", "ABC-1")])),
      ),
    )
  let state =
    apply_completion(
      state,
      effect_runner.Finished(
        2,
        effect_runner.RunningRefreshFinished(
          2,
          Error(error.LinearApiRequest("boom")),
        ),
      ),
    )
  let state =
    apply_completion(
      state,
      effect_runner.Finished(
        3,
        effect_runner.RetryRefreshFinished(
          "issue-1",
          3,
          Ok([issue("issue-1", "ABC-1")]),
        ),
      ),
    )
  let state =
    apply_completion(
      state,
      effect_runner.Finished(
        4,
        effect_runner.DispatchClaimValidationFinished(
          issue_id: "issue-1",
          generation: 4,
          result: Ok(issue("issue-1", "ABC-1")),
        ),
      ),
    )
  let state =
    apply_completion(
      state,
      effect_runner.Finished(
        5,
        effect_runner.HandoffClaimFinished("issue-1", "run-1", Ok(Nil)),
      ),
    )
  let state =
    apply_completion(
      state,
      effect_runner.Finished(
        6,
        effect_runner.HandoffSuccessFinished("issue-1", "run-1", Ok(Nil)),
      ),
    )
  let state =
    apply_completion(
      state,
      effect_runner.Finished(
        7,
        effect_runner.HandoffFailureFinished("issue-1", "run-1", Ok(Nil)),
      ),
    )
  let state =
    apply_completion(
      state,
      effect_runner.Finished(
        8,
        effect_runner.HandoffParkFinished("issue-1", Ok(Nil)),
      ),
    )
  let state =
    apply_completion(
      state,
      effect_runner.Finished(
        9,
        effect_runner.InvalidWorkflowReportFinished(
          "issue-1",
          "missing_workflow_label",
          "policy",
          Ok(effect_runner.InvalidWorkflowReportCommentAndState),
        ),
      ),
    )
  let state =
    apply_completion(
      state,
      effect_runner.Finished(
        10,
        effect_runner.ScheduledFailureReportFinished(
          10,
          publication,
          Ok(receipt),
        ),
      ),
    )
  let state =
    apply_completion(
      state,
      effect_runner.Finished(
        11,
        effect_runner.CleanupFinished("test/tmp/workspaces/ABC-1", Ok(Nil)),
      ),
    )

  assert state.events
    == [
      "candidate_fetch",
      "running_refresh",
      "retry_refresh",
      "dispatch_validation",
      "handoff_claim",
      "handoff_success",
      "handoff_failure",
      "handoff_park",
      "invalid_workflow",
      "scheduled_failure",
      "cleanup",
    ]
}

pub fn handle_completed_logs_crashes_before_dispatch_test() {
  let state =
    effect_completion_handler.handle_completed(
      context(new_state()),
      effect_runner.Crashed(
        1,
        effect_runner.ReportFailure(
          issue_task_ref("issue-1", "ABC-1"),
          "issue-1",
          issue("issue-1", "ABC-1"),
          worker_failure(),
          "run-1",
          "default",
          adapter.HandoffCapability(report: fn(_) { Ok(Nil) }),
        ),
        "boom",
      ),
    )

  assert state.events == ["crashed", "handoff_failure"]
}

pub fn handle_completed_crash_paths_match_daemon_error_outcomes_test() {
  let handoff = adapter.HandoffCapability(report: fn(_) { Ok(Nil) })

  let success_state =
    apply_daemon_completion(
      new_state(),
      effect_runner.Crashed(
        1,
        effect_runner.ReportSuccess(
          issue_task_ref("issue-1", "ABC-1"),
          "issue-1",
          issue("issue-1", "ABC-1"),
          worker_success(issue("issue-1", "ABC-1")),
          "run-1",
          "default",
          handoff,
        ),
        "boom",
      ),
    )
  let failure_state =
    apply_daemon_completion(
      new_state(),
      effect_runner.Crashed(
        2,
        effect_runner.ReportFailure(
          issue_task_ref("issue-1", "ABC-1"),
          "issue-1",
          issue("issue-1", "ABC-1"),
          worker_failure(),
          "run-1",
          "default",
          handoff,
        ),
        "boom",
      ),
    )
  let park_state =
    apply_daemon_completion(
      new_state(),
      effect_runner.Crashed(
        3,
        effect_runner.ReportPark(park_report(), handoff),
        "boom",
      ),
    )

  assert success_state.events == ["crashed", "handoff_success_failed"]
  assert failure_state.events == ["crashed", "handoff_failure_failed"]
  assert park_state.events == ["crashed", "handoff_park_failed"]
}

pub fn handle_completed_invalid_workflow_outcomes_match_daemon_behavior_test() {
  let state = new_state()
  let state =
    apply_daemon_completion(
      state,
      effect_runner.Finished(
        1,
        effect_runner.InvalidWorkflowReportFinished(
          "issue-1",
          "missing_workflow_label",
          "policy",
          Ok(effect_runner.InvalidWorkflowReportNoop),
        ),
      ),
    )
  let state =
    apply_daemon_completion(
      state,
      effect_runner.Finished(
        2,
        effect_runner.InvalidWorkflowReportFinished(
          "issue-1",
          "missing_workflow_label",
          "policy",
          Ok(effect_runner.InvalidWorkflowReportComment),
        ),
      ),
    )
  let state =
    apply_daemon_completion(
      state,
      effect_runner.Finished(
        3,
        effect_runner.InvalidWorkflowReportFinished(
          "issue-1",
          "missing_workflow_label",
          "policy",
          Ok(effect_runner.InvalidWorkflowReportState),
        ),
      ),
    )
  let state =
    apply_daemon_completion(
      state,
      effect_runner.Finished(
        4,
        effect_runner.InvalidWorkflowReportFinished(
          "issue-1",
          "missing_workflow_label",
          "policy",
          Ok(effect_runner.InvalidWorkflowReportCommentAndState),
        ),
      ),
    )
  let state =
    apply_daemon_completion(
      state,
      effect_runner.Finished(
        5,
        effect_runner.InvalidWorkflowReportFinished(
          "issue-1",
          "missing_workflow_label",
          "policy",
          Error(error.LinearApiRequest("boom")),
        ),
      ),
    )

  assert state.events
    == [
      "invalid_workflow_report_noop",
      "invalid_workflow_reported:comment",
      "invalid_workflow_reported:state",
      "invalid_workflow_reported:comment_and_state",
      "invalid_workflow_report_failed",
    ]
}

pub fn handle_completed_scheduled_failure_and_cleanup_match_daemon_behavior_test() {
  let publication = scheduled_failure_publication()
  let receipt = scheduled_failure_receipt()
  let state = new_state()
  let state =
    apply_daemon_completion(
      state,
      effect_runner.Finished(
        1,
        effect_runner.ScheduledFailureReportFinished(
          1,
          publication,
          Ok(receipt),
        ),
      ),
    )
  let state =
    apply_daemon_completion(
      state,
      effect_runner.Finished(
        2,
        effect_runner.ScheduledFailureReportFinished(
          2,
          publication,
          Error(error.LinearApiRequest("boom")),
        ),
      ),
    )
  let state =
    apply_daemon_completion(
      state,
      effect_runner.Finished(
        3,
        effect_runner.CleanupFinished("test/tmp/workspaces/ABC-1", Ok(Nil)),
      ),
    )
  let state =
    apply_daemon_completion(
      state,
      effect_runner.Finished(
        4,
        effect_runner.CleanupFinished(
          "test/tmp/workspaces/ABC-1",
          Error(error.WorkspaceIo("boom")),
        ),
      ),
    )

  assert state.events
    == [
      "scheduled_failure_report_finished",
      "scheduled_failure_report_failed",
      "workflow_cleanup_completed:deleted",
      "workspace_cleaned",
      "workflow_cleanup_completed:failed",
      "workspace_cleanup_failed",
    ]
}

pub fn crash_result_for_effect_maps_all_effect_variants_test() {
  let reason = "boom"
  let tracker_adapter =
    adapter.TrackerAdapter(
      kind: "test",
      display_name: "Test",
      task_source: adapter.TaskSourceCapability(
        fetch_candidates: fn(_) { Ok([]) },
        refresh_by_refs: fn(_) { Ok([]) },
        lookup_by_operator_ref: fn(_) { Ok(None) },
      ),
      comments: None,
      remote_commands: None,
      state_transitions: None,
      routing_metadata: None,
      links: None,
      handoff: None,
      scheduled_failures: None,
      readiness: None,
      smoke: None,
      attachments: None,
    )
  let handoff = adapter.HandoffCapability(report: fn(_) { Ok(Nil) })
  let publication = scheduled_failure_publication()

  assert effect_completion_handler.crash_result_for_effect(
      effect_runner.FetchCandidates(1, tracker_adapter),
      reason,
    )
    == effect_runner.CandidateFetchFinished(
      1,
      Error(error.LinearApiRequest(reason)),
    )
  assert effect_completion_handler.crash_result_for_effect(
      effect_runner.RefreshRunning(2, ["issue-1"], tracker_adapter),
      reason,
    )
    == effect_runner.RunningRefreshFinished(
      2,
      Error(error.LinearApiRequest(reason)),
    )
  assert effect_completion_handler.crash_result_for_effect(
      effect_runner.RefreshRetry("issue-1", 3, tracker_adapter),
      reason,
    )
    == effect_runner.RetryRefreshFinished(
      "issue-1",
      3,
      Error(error.LinearApiRequest(reason)),
    )
  assert effect_completion_handler.crash_result_for_effect(
      effect_runner.ValidateDispatchClaim("issue-1", 4, tracker_adapter),
      reason,
    )
    == effect_runner.DispatchClaimValidationFinished(
      issue_id: "issue-1",
      generation: 4,
      result: Error(
        effect_runner.DispatchValidationTrackerError(error.LinearApiRequest(
          reason,
        )),
      ),
    )
  assert effect_completion_handler.crash_result_for_effect(
      effect_runner.ClaimIssue(
        task_ref: issue_task_ref("issue-1", "ABC-1"),
        issue: issue("issue-1", "ABC-1"),
        workspace_path: "test/tmp/workspaces/ABC-1",
        run_id: "run-1",
        capability: handoff,
      ),
      reason,
    )
    == effect_runner.HandoffClaimFinished(
      "issue-1",
      "run-1",
      Error(error.LinearApiRequest(reason)),
    )
  assert effect_completion_handler.crash_result_for_effect(
      effect_runner.ReportSuccess(
        issue_task_ref("issue-1", "ABC-1"),
        "issue-1",
        issue("issue-1", "ABC-1"),
        worker_success(issue("issue-1", "ABC-1")),
        "run-1",
        "default",
        handoff,
      ),
      reason,
    )
    == effect_runner.HandoffSuccessFinished(
      "issue-1",
      "run-1",
      Error(error.LinearApiRequest(reason)),
    )
  assert effect_completion_handler.crash_result_for_effect(
      effect_runner.ReportFailure(
        issue_task_ref("issue-1", "ABC-1"),
        "issue-1",
        issue("issue-1", "ABC-1"),
        worker_failure(),
        "run-1",
        "default",
        handoff,
      ),
      reason,
    )
    == effect_runner.HandoffFailureFinished(
      "issue-1",
      "run-1",
      Error(error.LinearApiRequest(reason)),
    )
  assert effect_completion_handler.crash_result_for_effect(
      effect_runner.ReportPark(park_report(), handoff),
      reason,
    )
    == effect_runner.HandoffParkFinished(
      "issue-1",
      Error(error.LinearApiRequest(reason)),
    )
  assert effect_completion_handler.crash_result_for_effect(
      effect_runner.ReportInvalidWorkflow(
        issue("issue-1", "ABC-1"),
        workflow_policy.MissingWorkflowLabel,
        "missing_workflow_label",
        "policy",
        config.default_linear_contract_config(),
        None,
        None,
      ),
      reason,
    )
    == effect_runner.InvalidWorkflowReportFinished(
      "issue-1",
      "missing_workflow_label",
      "policy",
      Error(error.LinearApiRequest(reason)),
    )
  assert effect_completion_handler.crash_result_for_effect(
      effect_runner.ReportScheduledFailure(
        10,
        publication,
        scheduled_failure_capability(),
      ),
      reason,
    )
    == effect_runner.ScheduledFailureReportFinished(
      10,
      publication,
      Error(error.LinearApiRequest(reason)),
    )
  assert effect_completion_handler.crash_result_for_effect(
      effect_runner.CleanupWorkspace(
        root: "test/tmp/workspaces",
        workspace_path: "test/tmp/workspaces/ABC-1",
        hooks: config.default_hooks_config(),
        cleanup: fn(_, _, _) { Ok(Nil) },
      ),
      reason,
    )
    == effect_runner.CleanupFinished(
      "test/tmp/workspaces/ABC-1",
      Error(error.WorkspaceIo(reason)),
    )
}

type TestState {
  TestState(events: List(String))
}

fn new_state() -> TestState {
  TestState(events: [])
}

fn apply_completion(
  state: TestState,
  completion: effect_runner.Completion,
) -> TestState {
  effect_completion_handler.handle_completed(context(state), completion)
}

fn apply_daemon_completion(
  state: TestState,
  completion: effect_runner.Completion,
) -> TestState {
  effect_completion_handler.handle_completed(daemon_context(state), completion)
}

fn context(state: TestState) -> effect_completion_handler.Context(TestState) {
  effect_completion_handler.context(
    state: state,
    log_side_effect_crashed: fn(state, _, _) { append_event(state, "crashed") },
    result_handlers: effect_completion_handler.result_handlers(
      candidate_fetch_finished: fn(state, _, _) {
        append_event(state, "candidate_fetch")
      },
      running_refresh_finished: fn(state, _, _) {
        append_event(state, "running_refresh")
      },
      retry_refresh_finished: fn(state, _, _, _) {
        append_event(state, "retry_refresh")
      },
      dispatch_claim_validation_finished: fn(state, _, _, _) {
        append_event(state, "dispatch_validation")
      },
      handoff_claim_finished: fn(state, _, _, _) {
        append_event(state, "handoff_claim")
      },
      handoff_success_finished: fn(state, _, _) {
        append_event(state, "handoff_success")
      },
      handoff_failure_finished: fn(state, _, _) {
        append_event(state, "handoff_failure")
      },
      handoff_park_finished: fn(state, _, _) {
        append_event(state, "handoff_park")
      },
      invalid_workflow_report_finished: fn(state, _, _, _, _) {
        append_event(state, "invalid_workflow")
      },
      scheduled_failure_report_finished: fn(state, _, _, _) {
        append_event(state, "scheduled_failure")
      },
      cleanup_finished: fn(state, _, _) { append_event(state, "cleanup") },
    ),
  )
}

fn daemon_context(
  state: TestState,
) -> effect_completion_handler.Context(TestState) {
  effect_completion_handler.context(
    state: state,
    log_side_effect_crashed: fn(state, _, _) { append_event(state, "crashed") },
    result_handlers: effect_completion_handler.result_handlers(
      candidate_fetch_finished: fn(state, _, _) { state },
      running_refresh_finished: fn(state, _, _) { state },
      retry_refresh_finished: fn(state, _, _, _) { state },
      dispatch_claim_validation_finished: fn(state, _, _, _) { state },
      handoff_claim_finished: fn(state, _, _, _) { state },
      handoff_success_finished: fn(state, _, result) {
        case result {
          Ok(Nil) -> state
          Error(_) -> append_event(state, "handoff_success_failed")
        }
      },
      handoff_failure_finished: fn(state, _, result) {
        case result {
          Ok(Nil) -> state
          Error(_) -> append_event(state, "handoff_failure_failed")
        }
      },
      handoff_park_finished: fn(state, _, result) {
        case result {
          Ok(Nil) -> state
          Error(_) -> append_event(state, "handoff_park_failed")
        }
      },
      invalid_workflow_report_finished: fn(state, _, _, _, result) {
        case result {
          Ok(effect_runner.InvalidWorkflowReportNoop) ->
            append_event(state, "invalid_workflow_report_noop")
          Ok(outcome) ->
            append_event(
              state,
              "invalid_workflow_reported:"
                <> invalid_workflow_outcome_name(outcome),
            )
          Error(_) -> append_event(state, "invalid_workflow_report_failed")
        }
      },
      scheduled_failure_report_finished: fn(state, _, _, result) {
        case result {
          Ok(_) -> append_event(state, "scheduled_failure_report_finished")
          Error(_) -> append_event(state, "scheduled_failure_report_failed")
        }
      },
      cleanup_finished: fn(state, _, result) {
        case result {
          Ok(Nil) ->
            append_event(
              append_event(state, "workflow_cleanup_completed:deleted"),
              "workspace_cleaned",
            )
          Error(_) ->
            append_event(
              append_event(state, "workflow_cleanup_completed:failed"),
              "workspace_cleanup_failed",
            )
        }
      },
    ),
  )
}

fn invalid_workflow_outcome_name(
  outcome: effect_runner.InvalidWorkflowReportOutcome,
) -> String {
  case outcome {
    effect_runner.InvalidWorkflowReportNoop -> "noop"
    effect_runner.InvalidWorkflowReportComment -> "comment"
    effect_runner.InvalidWorkflowReportState -> "state"
    effect_runner.InvalidWorkflowReportCommentAndState -> "comment_and_state"
  }
}

fn append_event(state: TestState, event: String) -> TestState {
  TestState(events: list.append(state.events, [event]))
}

fn issue(id: String, identifier: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: Some(1),
    state: issue_state.from_string_unchecked("Todo"),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn issue_task_ref(remote_id: String, key: String) -> task.TaskRef {
  task.TaskRef(
    backend_kind: "linear",
    remote_id: remote_id,
    key: Some(key),
    url: None,
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

fn park_report() -> adapter.ParkReport {
  adapter.ParkReport(
    task: task.TaskRef(
      backend_kind: "linear",
      remote_id: "issue-1",
      key: Some("ABC-1"),
      url: None,
    ),
    issue_identifier: "ABC-1",
    reason: "waiting_on_input",
    release_policy: Some("manual"),
    run_id: Some("run-1"),
  )
}

fn scheduled_failure_publication() -> adapter.ScheduledFailurePublication {
  adapter.ScheduledFailurePublication(
    job_id: "job-1",
    workflow_id: "default",
    due_at_ms: 100,
    run_id: "run-1",
    attempt: 1,
    max_attempts: 3,
    reason: "boom",
    run_root: Some("test/tmp/workspaces/run-1"),
    session_id: Some("session-1"),
    dedupe_key: "dedupe",
    title: "Scheduled failure",
    body: "failed",
    labels: [],
    target_state_name: None,
    previous_task_remote_id: None,
  )
}

fn scheduled_failure_receipt() -> adapter.ScheduledFailureReceipt {
  adapter.ScheduledFailureReceipt(
    task: task.TaskRef(
      backend_kind: "linear",
      remote_id: "issue-2",
      key: Some("ABC-2"),
      url: None,
    ),
    created: True,
    comment_id: None,
  )
}

fn scheduled_failure_capability() -> adapter.ScheduledFailureCapability {
  adapter.ScheduledFailureCapability(publish: fn(_) {
    Ok(scheduled_failure_receipt())
  })
}
