import gleam/dict
import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/config/types as config_types
import scherzo/error
import scherzo/orchestrator/daemon
import scherzo/result_artifact
import scherzo/session/hub
import scherzo/session/tokens as session_tokens
import scherzo/tracker
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
import scherzo/work_item_invalidation
import scherzo/workflow_attempt
import scherzo/workflow_run
import simplifile
import support/fake_tracker_adapter
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

fn workflow_text(root: String) -> String {
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
  concurrency: 1
  sessions_per_task: 3
  runtime:
    type: pi
    pi:
      executable: fake
task_updates:
  enabled: true
  comment_on: [claim, success, failure, park]
task_routing:
  labels:
    require_exactly_one: false
    default_workflow: implementation
workflows:
  implementation: workflows/implementation.yaml
  execplan: workflows/execplan.yaml
"
}

fn write_workflow(dir: String) -> #(String, String) {
  test_helpers.reset_dir(dir)
  let root = dir <> "/workspaces"
  #(write_workflow_files(dir, workflow_text(root)), root)
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
      workflow_file_text("implementation"),
    )
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/execplan.yaml",
      workflow_file_text("execplan"),
    )
  config_path
}

fn workflow_file_text(id: String) -> String {
  "version: 1
id: " <> id <> "
steps:
  - id: implement
    kind: agent
    prompt: prompts/task.md
    run_in: main
"
}

fn adapter_recording_handoff(
  handoff_subject: process.Subject(adapter.HandoffEvent),
) -> adapter.TrackerAdapter {
  let base = fake_tracker_adapter.seam_adapter()
  let assert Some(adapter.HandoffCapability(report: base_report)) = base.handoff
  adapter.TrackerAdapter(
    ..base,
    handoff: Some(
      adapter.HandoffCapability(report: fn(event) {
        process.send(handoff_subject, event)
        base_report(event)
      }),
    ),
  )
}

fn dependencies(
  handoff_subject: process.Subject(adapter.HandoffEvent),
  log_subject: process.Subject(String),
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
    make_tracker_adapter: fn(_) { adapter_recording_handoff(handoff_subject) },
    cleanup: fn(_, _, _) { Ok(Nil) },
    logger: fn(_, event, _, _) {
      process.send(log_subject, event)
      Ok(Nil)
    },
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

fn record_agent_refresh(
  subject: process.Subject(String),
  tracker_client: tracker.Client,
  issue: tracker_issue.Issue,
) -> Nil {
  case tracker_client.fetch_issue_states_by_ids([issue.id]) {
    Ok([refreshed]) -> process.send(subject, "agent_refresh:" <> refreshed.id)
    _ -> process.send(subject, "agent_refresh_failed")
  }
}

pub fn daemon_poll_emits_work_item_invalidation_for_candidate_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-fake-adapter-invalidation")
  let handoff_subject = process.new_subject()
  let log_subject = process.new_subject()
  let invalidation_subject = process.new_subject()
  let agent_started_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let base_deps =
    dependencies(handoff_subject, log_subject, fn(_, _, _, _, _, _, _, _) {
      process.send(agent_started_subject, Nil)
      test_async.block_until_released(worker_barrier)
      Error(agent_types.WorkerFailure(
        reason: error.PiFailed(error.PiProtocolError("released")),
        workspace_path: None,
        tokens: session_tokens.zero_token_totals(),
        final_issue: None,
      ))
    })
  let deps =
    daemon.RuntimeDependencies(
      ..base_deps,
      emit_work_item_invalidation: fn(_, event) {
        process.send(invalidation_subject, event)
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(started.data, 5000)

  process.send(started.data, daemon.PollTick(1))

  let assert Ok(Nil) = process.receive(agent_started_subject, within: 5000)
  test_async.release_barrier(worker_barrier)
  let assert Ok(event) =
    wait_for_invalidation_source(
      invalidation_subject,
      work_item_invalidation.PollRefresh,
      20,
    )
  assert_fake_task_invalidation(event)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_dispatch_emits_tracker_and_workflow_invalidation_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-fake-adapter-dispatch-invalidation")
  let handoff_subject = process.new_subject()
  let log_subject = process.new_subject()
  let invalidation_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let base_deps =
    dependencies(handoff_subject, log_subject, fn(_, _, _, _, _, _, _, _) {
      test_async.block_until_released(worker_barrier)
      Error(agent_types.WorkerFailure(
        reason: error.PiFailed(error.PiProtocolError("released")),
        workspace_path: None,
        tokens: session_tokens.zero_token_totals(),
        final_issue: None,
      ))
    })
  let deps =
    daemon.RuntimeDependencies(
      ..base_deps,
      emit_work_item_invalidation: fn(_, event) {
        process.send(invalidation_subject, event)
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))

  let assert Ok(tracker_event) =
    wait_for_invalidation_source(
      invalidation_subject,
      work_item_invalidation.TrackerRefresh,
      50,
    )
  assert_fake_task_invalidation(tracker_event)
  let assert Ok(workflow_event) =
    wait_for_invalidation_source(
      invalidation_subject,
      work_item_invalidation.WorkflowObserved,
      50,
    )
  assert_fake_task_invalidation(workflow_event)

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn fake_non_linear_adapter_dispatches_validates_and_hands_off_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-fake-adapter-dispatch")
  let handoff_subject = process.new_subject()
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let deps =
    dependencies(
      handoff_subject,
      log_subject,
      fn(issue, _, _, _, tracker_client, _, _, _) {
        record_agent_refresh(log_subject, tracker_client, issue)
        process.send(log_subject, "agent_run:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("released")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))

  let assert Ok(adapter.HandoffClaim(task: claimed_task, ..)) =
    process.receive(handoff_subject, within: 5000)
  assert claimed_task.ref == fake_tracker_adapter.task_ref()
  assert wait_for_log(log_subject, "agent_refresh:card-1", 50)
  assert wait_for_log(log_subject, "agent_run:card-1", 50)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  let assert [running] = dict.values(snapshot.running)
  assert running.task.ref == fake_tracker_adapter.task_ref()

  test_async.release_barrier(worker_barrier)
  let assert Ok(adapter.HandoffFailure(task: failed_task, ..)) =
    process.receive(handoff_subject, within: 5000)
  assert failed_task.ref == fake_tracker_adapter.task_ref()
  let assert Ok(failed_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.values(failed_snapshot.retry_attempts) == []
  let assert [parked] = dict.values(failed_snapshot.parked)
  assert parked.task_ref == fake_tracker_adapter.task_ref()
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn fake_non_linear_active_workflow_success_releases_without_retry_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-fake-adapter-active-success")
  let handoff_subject = process.new_subject()
  let log_subject = process.new_subject()
  let deps =
    dependencies(
      handoff_subject,
      log_subject,
      fn(issue, _, _, _, tracker_client, _, _, _) {
        record_agent_refresh(log_subject, tracker_client, issue)
        process.send(log_subject, "agent_run:" <> issue.id)
        Ok(agent_types.WorkerSuccess(
          final_issue: Some(issue),
          final_classification: agent_types.FinalActive,
          workspace_path: "test/tmp/fake-adapter-active-workspace",
          tokens: session_tokens.zero_token_totals(),
          turns: 1,
          result: result_artifact.from_final_response(
            Some("active"),
            False,
            "test",
          ),
        ))
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  let assert Ok(adapter.HandoffClaim(task: claimed_task, ..)) =
    process.receive(handoff_subject, within: 5000)
  assert claimed_task.ref == fake_tracker_adapter.task_ref()
  assert wait_for_log(log_subject, "agent_refresh:card-1", 50)
  assert wait_for_log(log_subject, "agent_run:card-1", 50)
  let assert Ok(adapter.HandoffSuccess(success_task, _, _, _)) =
    process.receive(handoff_subject, within: 5000)
  assert success_task.ref == fake_tracker_adapter.task_ref()
  assert !wait_for_log(log_subject, "retry_scheduled", 1)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.values(snapshot.retry_attempts) == []
  assert dict.values(snapshot.running) == []
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

fn wait_for_invalidation_source(
  subject: process.Subject(work_item_invalidation.Event),
  source: work_item_invalidation.Source,
  attempts: Int,
) -> Result(work_item_invalidation.Event, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case process.receive(subject, within: 250) {
        Ok(event) ->
          case event.source == source {
            True -> Ok(event)
            False -> wait_for_invalidation_source(subject, source, attempts - 1)
          }
        Error(_) -> wait_for_invalidation_source(subject, source, attempts - 1)
      }
  }
}

fn assert_fake_task_invalidation(event: work_item_invalidation.Event) -> Nil {
  let assert [ref] = event.task_refs
  assert ref.provider == fake_tracker_adapter.backend_kind
  assert ref.id == "card-1"
  assert ref.display_id == Some("CARD-1")
  assert !event.has_unknown_refs
}

fn wait_for_log(
  subject: process.Subject(String),
  expected: String,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case process.receive(subject, within: 250) {
        Ok(actual) ->
          case actual == expected {
            True -> True
            False -> wait_for_log(subject, expected, attempts - 1)
          }
        Error(_) -> wait_for_log(subject, expected, attempts - 1)
      }
  }
}
