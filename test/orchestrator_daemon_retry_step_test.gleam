import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import scherzo/agent/types as agent_types
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/error
import scherzo/handoff
import scherzo/hash
import scherzo/orchestrator/daemon
import scherzo/path
import scherzo/session/hub
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
    "run_started",
    "known_workspace",
    "issue_counter_updated",
  ])

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

pub fn retry_step_accepts_non_active_issue_state_for_retained_run_test() {
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

  assert command.status_reason(result.status) == None
  assert command.status_to_string(result.status) == "applied"
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
  kind: linear
  api_key: test-key
  project_slug: TEST
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
  let base_records = [
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
  let records = case parked {
    True ->
      list.append(base_records, [
        record.with_id(
          "issue-parked",
          9,
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

fn recovery_append_state(
  _log_subject: process.Subject(String),
  root: String,
) -> String {
  case
    contains_kind_sequence(root, [
      "workflow_repair_requested",
      "step_attempt_superseded",
      "workflow_run_started",
      "run_started",
      "known_workspace",
      "issue_counter_updated",
    ])
  {
    True -> "retry_step_ledger_ready"
    False -> "retry_step_ledger_missing"
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
