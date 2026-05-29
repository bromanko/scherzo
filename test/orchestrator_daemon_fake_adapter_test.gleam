import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/option.{type Option, None, Some}
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/config/types as config_types
import scherzo/error
import scherzo/orchestrator/daemon
import scherzo/session/hub
import scherzo/session/tokens as session_tokens
import scherzo/tracker
import scherzo/tracker/adapter
import scherzo/tracker/issue as tracker_issue
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

fn workflow_text(root: String, max_retry_attempts: Int) -> String {
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
  retries:
    attempts: " <> int.to_string(max_retry_attempts) <> "
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

fn write_workflow(dir: String, max_retry_attempts: Int) -> #(String, String) {
  test_helpers.reset_dir(dir)
  let root = dir <> "/workspaces"
  #(write_workflow_files(dir, workflow_text(root, max_retry_attempts)), root)
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

pub fn fake_non_linear_adapter_dispatches_validates_and_hands_off_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-fake-adapter-dispatch", 2)
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
    process.receive(handoff_subject, within: 1000)
  assert claimed_task.ref == fake_tracker_adapter.task_ref()
  assert wait_for_log(log_subject, "agent_refresh:card-1", 20)
  assert wait_for_log(log_subject, "agent_run:card-1", 20)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  let assert [running] = dict.values(snapshot.running)
  assert running.task.ref == fake_tracker_adapter.task_ref()

  test_async.release_barrier(worker_barrier)
  let assert Ok(adapter.HandoffFailure(task: failed_task, ..)) =
    process.receive(handoff_subject, within: 1000)
  assert failed_task.ref == fake_tracker_adapter.task_ref()
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn fake_non_linear_retry_survives_startup_recovery_and_refreshes_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-fake-adapter-recovery", 3)
  let first_handoff_subject = process.new_subject()
  let first_log_subject = process.new_subject()
  let first_deps =
    dependencies(
      first_handoff_subject,
      first_log_subject,
      fn(issue, _, _, _, tracker_client, _, _, _) {
        record_agent_refresh(first_log_subject, tracker_client, issue)
        process.send(first_log_subject, "agent_run:" <> issue.id)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("first failure")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(first) = daemon.start(Some(workflow_path), first_deps)
  process.send(first.data, daemon.PollTick(1))
  let _ = process.receive(first_handoff_subject, within: 1000)
  assert wait_for_log(first_log_subject, "retry_scheduled", 20)
  let assert Ok(first_snapshot) = daemon.get_snapshot(first.data, 1000)
  let assert [first_retry] = dict.values(first_snapshot.retry_attempts)
  assert first_retry.task_ref == fake_tracker_adapter.task_ref()
  assert daemon.shutdown(first.data, 1000) == Ok(Nil)

  let second_handoff_subject = process.new_subject()
  let second_log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let second_deps =
    dependencies(
      second_handoff_subject,
      second_log_subject,
      fn(issue, _, _, _, tracker_client, _, _, _) {
        record_agent_refresh(second_log_subject, tracker_client, issue)
        process.send(second_log_subject, "agent_run:" <> issue.id)
        test_async.block_until_released(worker_barrier)
        Error(agent_types.WorkerFailure(
          reason: error.PiFailed(error.PiProtocolError("released")),
          workspace_path: None,
          tokens: session_tokens.zero_token_totals(),
          final_issue: None,
        ))
      },
    )
  let assert Ok(second) = daemon.start(Some(workflow_path), second_deps)
  let assert Ok(recovered_snapshot) = daemon.get_snapshot(second.data, 1000)
  let assert [recovered_retry] = dict.values(recovered_snapshot.retry_attempts)

  process.send(
    second.data,
    daemon.RetryTick(recovered_retry.issue_id, recovered_retry.timer_generation),
  )
  let assert Ok(adapter.HandoffClaim(task: retried_task, ..)) =
    process.receive(second_handoff_subject, within: 1000)
  assert retried_task.ref == fake_tracker_adapter.task_ref()
  assert wait_for_log(second_log_subject, "agent_refresh:card-1", 20)
  assert wait_for_log(second_log_subject, "agent_run:card-1", 20)
  let assert Ok(running_snapshot) = daemon.get_snapshot(second.data, 1000)
  let assert [running] = dict.values(running_snapshot.running)
  assert running.task.ref == fake_tracker_adapter.task_ref()

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(second.data, 1000) == Ok(Nil)
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
