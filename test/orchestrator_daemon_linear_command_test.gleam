import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/config/types as config_types
import scherzo/control/command
import scherzo/error
import scherzo/orchestrator/daemon
import scherzo/runtime/state as orchestrator_state
import scherzo/session/hub
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger
import scherzo/state/record
import scherzo/tracker
import scherzo/tracker/adapter_legacy
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
import scherzo/workflow_run
import simplifile
import support/test_helpers
import test_async

fn prompt_text(mode: workflow_attempt.AgentPromptMode) -> String {
  case mode {
    workflow_attempt.OriginalPrompt(prompt) -> prompt
    workflow_attempt.StructuredOutputRetryPrompt(prompt) -> prompt
    workflow_attempt.StepRecoveryPrompt(prompt) -> prompt
    workflow_attempt.RecoveryPrompt(prompt) -> prompt
  }
}

fn workflow_text_with_limits(
  root: String,
  max_concurrent_agents: Int,
  max_retry_attempts: Int,
  max_sessions_per_issue: Int,
) -> String {
  "version: 1
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
  concurrency: " <> int_to_string(max_concurrent_agents) <> "
  sessions_per_task: " <> int_to_string(max_sessions_per_issue) <> "
  retries:
    attempts: " <> int_to_string(max_retry_attempts) <> "
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
"
}

fn write_workflow_with_limits(
  dir: String,
  max_concurrent_agents: Int,
  max_retry_attempts: Int,
  max_sessions_per_issue: Int,
) -> #(String, String) {
  test_helpers.reset_dir(dir)
  let root = dir <> "/workspaces"
  #(
    write_workflow_files(
      dir,
      workflow_text_with_limits(
        root,
        max_concurrent_agents,
        max_retry_attempts,
        max_sessions_per_issue,
      ),
    ),
    root,
  )
}

fn write_workflow_files(dir: String, config_text: String) -> String {
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) = simplifile.write(config_path, config_text)
  let assert Ok(Nil) = simplifile.write(prompt_dir <> "/task.md", "Prompt")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: implement
    kind: agent
    prompt: prompts/task.md
    run_in: main
",
    )
  config_path
}

fn seed_failed_publication_without_output_manifest(root: String) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "workflow-started",
          1000,
          record.WorkflowRunStarted(
            run_id: "run-1",
            workflow_id: "implementation",
            workflow_fingerprint: "wf-1",
            issue_id: "issue-1",
            issue_identifier: "LIV-826",
            issue_fingerprint: "issue-fingerprint",
            observed_updated_at_ms: 999,
            run_root: root <> "/runs/run-1",
          ),
        ),
        record.with_id(
          "publication-failed",
          1020,
          record.PublicationAttemptRecorded(
            run_id: "run-1",
            workflow_id: "implementation",
            publication_id: "review_doc",
            series_id: "issue-1:implementation:review_doc",
            attempt_id: "failed-1",
            status: "failed",
            required: True,
            retryable: True,
            retry_execution_available: True,
            version_id: Some("version-1"),
            manifest_ref: None,
            manifest_sha256: None,
            manifest_bytes: None,
            error_code: Some("git_push_failed"),
            error_message: Some("previous push failed"),
          ),
        ),
      ],
      True,
    )
  Nil
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

fn tracker_with(candidate: tracker_issue.Issue) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([candidate]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
  )
}

fn dependencies(
  tracker_client: tracker.Client,
  agent_runner: fn(
    tracker_issue.Issue,
    Option(Int),
    String,
    config_types.EffectiveConfig,
    tracker.Client,
    fn(String, agent_types.RunnerUpdate) -> Nil,
    process.Subject(worker_command.Command),
    fn() -> Nil,
  ) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..daemon.default_dependencies(),
    make_tracker_adapter: fn(_) {
      adapter_legacy.adapter_from_legacy_client(tracker_client, "linear")
    },
    cleanup: fn(_, _, _) { Ok(Nil) },
    logger: fn(_, _, _, _) { Ok(Nil) },
    now_ms: fn() { 42 },
    send_after: fn(_, delay, _) { daemon.TestTimer(delay) },
    cancel_timer: fn(_) { Nil },
    workflow_run_dependencies: workflow_run.Dependencies(
      ..workflow_run.default_dependencies(),
      agent_step: fn(
        issue,
        _context,
        prompt_mode,
        _attempt_context,
        effective,
        tracker_client,
        emit_update,
        command_ready,
        _record_pi_session,
      ) {
        let command_subject = process.new_subject()
        agent_runner(
          issue,
          None,
          prompt_text(prompt_mode),
          effective,
          tracker_client,
          fn(_, update) { emit_update(update) },
          command_subject,
          fn() { command_ready(command_subject) },
        )
      },
    ),
    start_event_hub: fn() { hub.start(50, fn() { 42 }) },
    make_control_token: fn() { Ok("test-token") },
    start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
    stop_control_server: fn(_) { Nil },
  )
}

pub fn retry_artifact_publication_rejects_missing_output_manifest_test() {
  let #(workflow_path, root) =
    write_workflow_with_limits(
      "test/tmp/daemon-artifact-publication-retry-missing-output",
      1,
      3,
      3,
    )
  seed_failed_publication_without_output_manifest(root)
  let deps =
    dependencies(
      tracker_with(issue("issue-1", "LIV-826", "Todo")),
      fn(_, _, _, _, _, _, _, _) {
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("unused")),
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
      command.RetryArtifactPublication("run-1", Some("review_doc")),
      1000,
    )

  assert command.status_to_string(result.status) == "not_found"
}

pub fn retry_issue_identifier_dispatches_tracker_candidate_test() {
  let candidate = issue("retry-identifier-issue", "LIV-724", "Todo")
  let #(workflow_path, _root) =
    write_workflow_with_limits(
      "test/tmp/daemon-linear-retry-identifier",
      1,
      3,
      3,
    )
  let worker_barrier = test_async.new_barrier()
  let deps =
    dependencies(tracker_with(candidate), fn(_, _, _, _, _, _, _, _) {
      test_async.block_until_released(worker_barrier)
      Error(agent_types.WorkerFailure(
        reason: error.PiFailed(error.PiProtocolError("stopped")),
        workspace_path: None,
        tokens: session_tokens.zero_token_totals(),
        final_issue: None,
      ))
    })
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.RetryIssue(command.IssueIdentifier("LIV-724")),
      1000,
    )
  assert result.command == "retry"
  assert result.status == command.Applied
  assert result.target == Some("LIV-724")

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn park_and_unpark_issue_identifier_round_trip_test() {
  let candidate = issue("parked-identifier-issue", "LIV-556", "Todo")
  let #(workflow_path, _root) =
    write_workflow_with_limits(
      "test/tmp/daemon-linear-park-unpark-identifier",
      0,
      1,
      1,
    )
  let deps =
    dependencies(tracker_with(candidate), fn(_, _, _, _, _, _, _, _) {
      Error(agent_types.WorkerFailure(
        reason: error.PiFailed(error.PiProtocolError("unused")),
        workspace_path: None,
        tokens: session_tokens.zero_token_totals(),
        final_issue: None,
      ))
    })
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(parked) =
    daemon.apply_operator_command(
      started.data,
      command.ParkIssue(command.IssueIdentifier("LIV-556"), "manual"),
      1000,
    )
  assert parked.command == "park"
  assert parked.status == command.Applied
  let identity = orchestrator_state.issue_identity(candidate)
  let assert Ok(parked_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(parked_snapshot.parked, identity)

  let assert Ok(unparked) =
    daemon.apply_operator_command(
      started.data,
      command.UnparkIssue(command.IssueIdentifier("LIV-556")),
      1000,
    )
  assert unparked.command == "unpark"
  assert unparked.status == command.Applied
  let assert Ok(unparked_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert !dict.has_key(unparked_snapshot.parked, identity)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

fn int_to_string(value: Int) -> String {
  int.to_string(value)
}
