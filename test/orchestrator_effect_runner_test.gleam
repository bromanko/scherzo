import gleam/dict
import gleam/erlang/process
import gleam/option.{None, Some}
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/error
import scherzo/orchestrator/effect_runner
import scherzo/orchestrator/outbox_effects
import scherzo/result_artifact
import scherzo/session/tokens as session_tokens
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_policy
import support/expected_crash
import test_async

fn hooks() -> config_types.HooksConfig {
  config_types.HooksConfig(
    after_create: None,
    before_run: None,
    after_run: None,
    before_remove: None,
    timeout_ms: 1000,
  )
}

fn issue() -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: "issue-id",
    identifier: "LIV-570",
    title: "Promote generic tracker handoff events",
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

fn worker_success(issue: tracker_issue.Issue) -> agent_types.WorkerSuccess {
  agent_types.WorkerSuccess(
    final_issue: Some(issue),
    final_classification: agent_types.FinalTerminal,
    workspace_path: "workspace/main",
    tokens: session_tokens.zero_token_totals(),
    turns: 1,
    result: result_artifact.from_final_response(
      Some("generic handoff success"),
      False,
      "agent_end_messages",
    ),
  )
}

fn worker_failure() -> agent_types.WorkerFailure {
  agent_types.WorkerFailure(
    reason: error.PiFailed(error.PiProtocolError("generic handoff failure")),
    workspace_path: Some("workspace/main"),
    tokens: session_tokens.zero_token_totals(),
    final_issue: None,
  )
}

fn outbox(task_ref: task.TaskRef, kind: String) -> outbox_effects.Intent {
  outbox_effects.Intent(
    outbox_id: "outbox-" <> kind,
    task_ref: outbox_effects.task_ref_fields(task_ref),
    outbox_kind: kind,
    dedupe_key: "test:" <> kind,
    payload_json: "{}",
  )
}

fn start_runner(
  completions: process.Subject(effect_runner.Completion),
) -> effect_runner.Handle {
  let assert Ok(handle) =
    effect_runner.start(
      effect_runner.Dependencies(max_concurrent: 1, notify: fn(completion) {
        process.send(completions, completion)
      }),
    )
  handle
}

fn invalid_workflow_contract(
  state: String,
) -> config_types.LinearContractConfig {
  config_types.LinearContractConfig(
    enabled: False,
    workflow_label_prefix: "workflow:",
    workflow_labels: [],
    support_labels: [],
    required_states: dict.new(),
    handoff_state_bindings: dict.new(),
    enforce_issue_workflow_labels: True,
    invalid_workflow_state_id: Some(state),
    invalid_workflow_state_target: Some(config_types.InvalidWorkflowStateName(
      state,
    )),
    comment_on_invalid_workflow: False,
  )
}

pub fn effect_runner_emits_generic_handoff_events_test() {
  let completions = process.new_subject()
  let events = process.new_subject()
  let runner = start_runner(completions)
  let capability =
    adapter.HandoffCapability(report: fn(event) {
      process.send(events, event)
      Ok(Nil)
    })
  let issue = issue()
  let expected_task = task.from_legacy_issue(issue)

  effect_runner.enqueue(
    runner,
    effect_runner.ClaimIssue(
      outbox: outbox(expected_task.ref, "claim"),
      task_ref: expected_task.ref,
      issue: issue,
      workspace_path: "workspace/main",
      run_id: "run-claim",
      capability: capability,
    ),
  )
  let assert Ok(adapter.HandoffClaim(claim_task, "workspace/main", "run-claim")) =
    process.receive(events, within: 1000)
  assert claim_task.ref == expected_task.ref
  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.HandoffClaimFinished(_, "issue-id", "run-claim", Ok(Nil)),
  )) = process.receive(completions, within: 1000)

  let success = worker_success(issue)
  effect_runner.enqueue(
    runner,
    effect_runner.ReportSuccess(
      outbox: outbox(expected_task.ref, "success"),
      task_ref: expected_task.ref,
      issue_id: issue.id,
      issue: issue,
      success: success,
      run_id: "run-success",
      workflow_id: "workflow:implementation",
      capability: capability,
    ),
  )
  let assert Ok(adapter.HandoffSuccess(
    success_task,
    observed_success,
    "run-success",
    "workflow:implementation",
  )) = process.receive(events, within: 1000)
  assert success_task.ref == expected_task.ref
  assert observed_success.workspace_path == success.workspace_path
  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.HandoffSuccessFinished(_, "issue-id", "run-success", Ok(Nil)),
  )) = process.receive(completions, within: 1000)

  let failure = worker_failure()
  effect_runner.enqueue(
    runner,
    effect_runner.ReportFailure(
      outbox: outbox(expected_task.ref, "failure"),
      task_ref: expected_task.ref,
      issue_id: issue.id,
      issue: issue,
      failure: failure,
      run_id: "run-failure",
      workflow_id: "workflow:implementation",
      capability: capability,
    ),
  )
  let assert Ok(adapter.HandoffFailure(
    failure_task,
    observed_failure,
    "run-failure",
    "workflow:implementation",
  )) = process.receive(events, within: 1000)
  assert failure_task.ref == expected_task.ref
  assert observed_failure.workspace_path == failure.workspace_path
  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.HandoffFailureFinished(_, "issue-id", "run-failure", Ok(Nil)),
  )) = process.receive(completions, within: 1000)

  let park_report =
    adapter.ParkReport(
      task: expected_task.ref,
      issue_identifier: issue.identifier,
      reason: "needs operator input",
      release_policy: Some("explicit_unpark_only"),
      run_id: Some("run-park"),
    )
  effect_runner.enqueue(
    runner,
    effect_runner.ReportPark(
      outbox: outbox(expected_task.ref, "park"),
      report: park_report,
      capability: capability,
    ),
  )
  let assert Ok(adapter.HandoffPark(observed_park)) =
    process.receive(events, within: 1000)
  assert observed_park == park_report
  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.HandoffParkFinished(_, "issue-id", Ok(Nil)),
  )) = process.receive(completions, within: 1000)

  assert effect_runner.shutdown(runner, 1000) == Ok(Nil)
}

pub fn effect_runner_uses_state_name_for_invalid_workflow_transition_test() {
  let completions = process.new_subject()
  let requests = process.new_subject()
  let runner = start_runner(completions)
  let state_transitions =
    adapter.StateTransitionCapability(transition: fn(request) {
      let adapter.StateTransitionRequest(
        task: requested_task,
        target_state_id: target_state_id,
        target_state_name: target_state_name,
        ..,
      ) = request
      process.send(requests, request)
      Ok(adapter.StateTransitionReceipt(
        task: requested_task,
        state: task.TaskState(
          id: target_state_id,
          name: target_state_name,
          category: task.Unknown,
        ),
      ))
    })

  effect_runner.enqueue(
    runner,
    effect_runner.ReportInvalidWorkflow(
      outbox: outbox(task.from_legacy_issue(issue()).ref, "invalid-workflow"),
      issue: issue(),
      violation: workflow_policy.MissingWorkflowLabel,
      violation_fingerprint: "violation-fingerprint",
      reporting_policy_fingerprint: "policy-fingerprint",
      contract_config: invalid_workflow_contract("Triage"),
      comments: None,
      state_transitions: Some(state_transitions),
    ),
  )

  let assert Ok(request) = process.receive(requests, within: 1000)
  let adapter.StateTransitionRequest(
    target_state_id: target_state_id,
    target_state_name: target_state_name,
    reason: reason,
    ..,
  ) = request
  assert target_state_id == None
  assert target_state_name == "Triage"
  assert reason == "invalid_workflow"
  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.InvalidWorkflowReportFinished(
      _,
      "issue-id",
      "violation-fingerprint",
      "policy-fingerprint",
      Ok(effect_runner.InvalidWorkflowReportState),
    ),
  )) = process.receive(completions, within: 1000)

  assert effect_runner.shutdown(runner, 1000) == Ok(Nil)
}

pub fn effect_runner_keeps_id_shaped_invalid_workflow_state_names_name_only_test() {
  let completions = process.new_subject()
  let requests = process.new_subject()
  let runner = start_runner(completions)
  let state_transitions =
    adapter.StateTransitionCapability(transition: fn(request) {
      let adapter.StateTransitionRequest(
        task: requested_task,
        target_state_id: target_state_id,
        target_state_name: target_state_name,
        ..,
      ) = request
      process.send(requests, request)
      Ok(adapter.StateTransitionReceipt(
        task: requested_task,
        state: task.TaskState(
          id: target_state_id,
          name: target_state_name,
          category: task.Unknown,
        ),
      ))
    })

  effect_runner.enqueue(
    runner,
    effect_runner.ReportInvalidWorkflow(
      outbox: outbox(task.from_legacy_issue(issue()).ref, "invalid-workflow"),
      issue: issue(),
      violation: workflow_policy.MissingWorkflowLabel,
      violation_fingerprint: "violation-fingerprint",
      reporting_policy_fingerprint: "policy-fingerprint",
      contract_config: invalid_workflow_contract("state-needs-workflow"),
      comments: None,
      state_transitions: Some(state_transitions),
    ),
  )

  let assert Ok(request) = process.receive(requests, within: 1000)
  let adapter.StateTransitionRequest(
    target_state_id: target_state_id,
    target_state_name: target_state_name,
    ..,
  ) = request
  assert target_state_id == None
  assert target_state_name == "state-needs-workflow"
  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.InvalidWorkflowReportFinished(
      _,
      "issue-id",
      "violation-fingerprint",
      "policy-fingerprint",
      Ok(effect_runner.InvalidWorkflowReportState),
    ),
  )) = process.receive(completions, within: 1000)

  assert effect_runner.shutdown(runner, 1000) == Ok(Nil)
}

pub fn effect_runner_uses_legacy_invalid_workflow_state_ids_when_explicit_test() {
  let completions = process.new_subject()
  let requests = process.new_subject()
  let runner = start_runner(completions)
  let state_transitions =
    adapter.StateTransitionCapability(transition: fn(request) {
      let adapter.StateTransitionRequest(
        task: requested_task,
        target_state_id: target_state_id,
        target_state_name: target_state_name,
        ..,
      ) = request
      process.send(requests, request)
      Ok(adapter.StateTransitionReceipt(
        task: requested_task,
        state: task.TaskState(
          id: target_state_id,
          name: target_state_name,
          category: task.Unknown,
        ),
      ))
    })
  let state_id = "state-needs-workflow"
  let contract =
    config_types.LinearContractConfig(
      ..invalid_workflow_contract(state_id),
      invalid_workflow_state_target: Some(config_types.InvalidWorkflowStateId(
        state_id,
      )),
    )

  effect_runner.enqueue(
    runner,
    effect_runner.ReportInvalidWorkflow(
      outbox: outbox(task.from_legacy_issue(issue()).ref, "invalid-workflow"),
      issue: issue(),
      violation: workflow_policy.MissingWorkflowLabel,
      violation_fingerprint: "violation-fingerprint",
      reporting_policy_fingerprint: "policy-fingerprint",
      contract_config: contract,
      comments: None,
      state_transitions: Some(state_transitions),
    ),
  )

  let assert Ok(request) = process.receive(requests, within: 1000)
  let adapter.StateTransitionRequest(
    target_state_id: target_state_id,
    target_state_name: target_state_name,
    ..,
  ) = request
  assert target_state_id == Some(state_id)
  assert target_state_name == state_id
  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.InvalidWorkflowReportFinished(
      _,
      "issue-id",
      "violation-fingerprint",
      "policy-fingerprint",
      Ok(effect_runner.InvalidWorkflowReportState),
    ),
  )) = process.receive(completions, within: 1000)

  assert effect_runner.shutdown(runner, 1000) == Ok(Nil)
}

pub fn effect_runner_runs_successful_effect_once_test() {
  let completions = process.new_subject()
  let started = process.new_subject()
  let runner = start_runner(completions)

  effect_runner.enqueue(
    runner,
    effect_runner.CleanupWorkspace(
      root: "root",
      workspace_path: "workspace",
      hooks: hooks(),
      cleanup: fn(_, _, _) {
        process.send(started, "cleanup_started")
        Ok(Nil)
      },
    ),
  )

  assert process.receive(started, within: 1000) == Ok("cleanup_started")
  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.CleanupFinished("workspace", Ok(Nil)),
  )) = process.receive(completions, within: 1000)
  test_async.assert_no_extra_message_within(completions, 50)
  assert effect_runner.shutdown(runner, 1000) == Ok(Nil)
}

pub fn effect_runner_reports_crash_and_drains_queue_test() {
  use <- expected_crash.suppressing([
    "test/orchestrator_effect_runner_test.gleam",
    "effect_runner_reports_crash_and_drains_queue_test",
    "boom",
  ])
  let completions = process.new_subject()
  let started = process.new_subject()
  let runner = start_runner(completions)

  effect_runner.enqueue(
    runner,
    effect_runner.CleanupWorkspace(
      root: "root",
      workspace_path: "first",
      hooks: hooks(),
      cleanup: fn(_, _, _) {
        process.send(started, "first_started")
        panic as "boom"
      },
    ),
  )
  effect_runner.enqueue(
    runner,
    effect_runner.CleanupWorkspace(
      root: "root",
      workspace_path: "second",
      hooks: hooks(),
      cleanup: fn(_, _, _) {
        process.send(started, "second_started")
        Ok(Nil)
      },
    ),
  )

  assert process.receive(started, within: 1000) == Ok("first_started")
  let assert Ok(effect_runner.Crashed(_, crashed_effect, reason)) =
    process.receive(completions, within: 1000)
  assert reason == "side_effect_crashed"
  case crashed_effect {
    effect_runner.CleanupWorkspace(_, "first", _, _) -> Nil
    _ -> panic as "unexpected crashed effect"
  }
  assert process.receive(started, within: 1000) == Ok("second_started")
  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.CleanupFinished("second", Ok(Nil)),
  )) = process.receive(completions, within: 1000)
  assert effect_runner.shutdown(runner, 1000) == Ok(Nil)
}

pub fn effect_runner_shutdown_waits_for_in_flight_effect_test() {
  let completions = process.new_subject()
  let runner = start_runner(completions)
  let barrier = test_async.new_barrier()
  let started = process.new_subject()
  let shutdown_result = process.new_subject()

  effect_runner.enqueue(
    runner,
    effect_runner.CleanupWorkspace(
      root: "root",
      workspace_path: "slow",
      hooks: hooks(),
      cleanup: fn(_, _, _) {
        process.send(started, "slow_started")
        test_async.block_until_released(barrier)
        Ok(Nil)
      },
    ),
  )

  assert process.receive(started, within: 1000) == Ok("slow_started")
  let _ =
    process.spawn_unlinked(fn() {
      process.send(shutdown_result, effect_runner.shutdown(runner, 1000))
    })

  test_async.assert_no_extra_message_within(shutdown_result, 50)

  test_async.release_barrier(barrier)

  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.CleanupFinished("slow", Ok(Nil)),
  )) = process.receive(completions, within: 1000)
  assert process.receive(shutdown_result, within: 1000) == Ok(Ok(Nil))
}

pub fn effect_runner_shutdown_drops_queued_effects_test() {
  let completions = process.new_subject()
  let runner = start_runner(completions)
  let barrier = test_async.new_barrier()
  let started = process.new_subject()
  let shutdown_result = process.new_subject()

  effect_runner.enqueue(
    runner,
    effect_runner.CleanupWorkspace(
      root: "root",
      workspace_path: "first",
      hooks: hooks(),
      cleanup: fn(_, _, _) {
        process.send(started, "first_started")
        test_async.block_until_released(barrier)
        Ok(Nil)
      },
    ),
  )
  effect_runner.enqueue(
    runner,
    effect_runner.CleanupWorkspace(
      root: "root",
      workspace_path: "second",
      hooks: hooks(),
      cleanup: fn(_, _, _) {
        process.send(started, "second_started")
        Ok(Nil)
      },
    ),
  )

  assert process.receive(started, within: 1000) == Ok("first_started")
  let _ =
    process.spawn_unlinked(fn() {
      process.send(shutdown_result, effect_runner.shutdown(runner, 1000))
    })

  test_async.assert_no_extra_message_within(started, 50)

  test_async.release_barrier(barrier)

  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.CleanupFinished("first", Ok(Nil)),
  )) = process.receive(completions, within: 1000)
  assert process.receive(shutdown_result, within: 1000) == Ok(Ok(Nil))
  test_async.assert_no_extra_message(started)
  test_async.assert_no_extra_message(completions)
}
