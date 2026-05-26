import gleam/erlang/process
import gleam/option.{None, Some}
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/error
import scherzo/orchestrator/effect_runner
import scherzo/result_artifact
import scherzo/session/tokens as session_tokens
import scherzo/task
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import support/expected_crash

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
    effect_runner.ClaimIssue(issue, "workspace/main", "run-claim", capability),
  )
  let assert Ok(adapter.HandoffClaim(claim_task, "workspace/main", "run-claim")) =
    process.receive(events, within: 1000)
  assert claim_task.ref == expected_task.ref
  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.HandoffClaimFinished("issue-id", "run-claim", Ok(Nil)),
  )) = process.receive(completions, within: 1000)

  let success = worker_success(issue)
  effect_runner.enqueue(
    runner,
    effect_runner.ReportSuccess(
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
    effect_runner.HandoffSuccessFinished("issue-id", "run-success", Ok(Nil)),
  )) = process.receive(completions, within: 1000)

  let failure = worker_failure()
  effect_runner.enqueue(
    runner,
    effect_runner.ReportFailure(
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
    effect_runner.HandoffFailureFinished("issue-id", "run-failure", Ok(Nil)),
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
    effect_runner.ReportPark(park_report, capability),
  )
  let assert Ok(adapter.HandoffPark(observed_park)) =
    process.receive(events, within: 1000)
  assert observed_park == park_report
  let assert Ok(effect_runner.Finished(
    _,
    effect_runner.HandoffParkFinished("issue-id", Ok(Nil)),
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
  case process.receive(completions, within: 50) {
    Error(_) -> Nil
    Ok(_) -> panic as "duplicate completion"
  }
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
