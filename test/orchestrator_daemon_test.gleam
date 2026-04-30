import birl
import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import scherzo/agent/runner
import scherzo/agent/worker_command
import scherzo/control/command
import scherzo/domain
import scherzo/error
import scherzo/handoff
import scherzo/linear
import scherzo/linear_triage
import scherzo/orchestrator/daemon
import scherzo/path
import scherzo/session/event as session_event
import scherzo/session/hub
import scherzo/step_artifact
import scherzo/tracker
import scherzo/workflow_policy
import scherzo/workflow_run
import scherzo/workspace_run
import simplifile

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn issue(id: String, identifier: String, state: String) -> domain.Issue {
  domain.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: Some(1),
    state: state,
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn workflow_text(root: String, max_concurrent: Int) -> String {
  workflow_text_with_linear_contract(root, max_concurrent, "")
}

fn enforcing_linear_contract_text() -> String {
  "linear_contract:\n  workflow_label_prefix: \"workflow:\"\n  workflow_labels: [bugfix, research]\n  enforce_issue_workflow_labels: true\n"
}

fn workflow_text_with_linear_contract(
  root: String,
  max_concurrent: Int,
  linear_contract_text: String,
) -> String {
  "---\ntracker:\n  kind: linear\n  api_key: test-key\n  project_slug: TEST\nworkspace:\n  root: "
  <> root
  <> "\nhooks:\n  before_run: \"true\"\npolling:\n  interval_ms: 1000\nagent:\n  max_concurrent_agents: "
  <> int_to_string(max_concurrent)
  <> "\n  max_retry_attempts: 3\n  max_sessions_per_issue: 2\npi:\n  command: fake\n"
  <> linear_contract_text
  <> "---\nPrompt\n"
}

fn write_workflow(dir: String, max_concurrent: Int) -> String {
  reset_dir(dir)
  let workflow_path = dir <> "/WORKFLOW.md"
  let root = dir <> "/workspaces"
  let assert Ok(Nil) =
    simplifile.write(workflow_path, workflow_text(root, max_concurrent))
  workflow_path
}

fn write_enforcing_workflow(dir: String, max_concurrent: Int) -> String {
  write_workflow_with_contract(
    dir,
    max_concurrent,
    enforcing_linear_contract_text(),
  )
}

fn write_yaml_agent_workflow(dir: String) -> String {
  reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let root = dir <> "/workspaces"
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  kind: linear\n  api_key: test-key\n  project_slug: TEST\n  active_states: [Todo]\n  terminal_states: [Done]\nworkspace:\n  root: "
        <> root
        <> "\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  workflows:\n    implementation: workflows/implementation.yaml\nagent:\n  max_concurrent_agents: 1\n",
    )
  let assert Ok(Nil) =
    simplifile.write(prompt_dir <> "/implement.md", "agent prompt")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: implement\n    kind: agent\n    prompt: prompts/implement.md\n    workspace: main\n",
    )
  config_path
}

fn write_yaml_workflow(dir: String, _marker: String) -> String {
  reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let root = dir <> "/workspaces"
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      "version: 1\ntracker:\n  kind: linear\n  api_key: test-key\n  project_slug: TEST\n  active_states: [Todo]\n  terminal_states: [Done]\nworkspace:\n  root: "
        <> root
        <> "\nrouting:\n  workflow_label_prefix: \"workflow:\"\n  require_exactly_one_workflow_label: true\n  workflows:\n    implementation: workflows/implementation.yaml\nagent:\n  max_concurrent_agents: 1\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1\nid: implementation\nsteps:\n  - id: final_test\n    kind: command\n    run: sh -c 'exit 1'\n    workspace: main\n",
    )
  config_path
}

fn write_workflow_with_contract(
  dir: String,
  max_concurrent: Int,
  linear_contract_text: String,
) -> String {
  reset_dir(dir)
  let workflow_path = dir <> "/WORKFLOW.md"
  let root = dir <> "/workspaces"
  let assert Ok(Nil) =
    simplifile.write(
      workflow_path,
      workflow_text_with_linear_contract(
        root,
        max_concurrent,
        linear_contract_text,
      ),
    )
  workflow_path
}

fn success(final: domain.Issue, workspace_path: String) -> runner.WorkerSuccess {
  runner.WorkerSuccess(
    final_issue: Some(final),
    final_classification: runner.FinalTerminal,
    workspace_path: workspace_path,
    tokens: domain.zero_token_totals(),
    turns: 1,
    result: domain.ResultArtifact(
      final_response: None,
      truncated: False,
      source: "none",
    ),
  )
}

fn base_dependencies(
  client: tracker.Client,
  log_subject: process.Subject(String),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    make_tracker: fn(_) { client },
    make_handoff: fn(_, _) { handoff.disabled_client() },
    make_linear_commands: fn(_) { disabled_linear_commands() },
    make_triage: fn(_, _) { linear_triage.disabled_client() },
    agent_runner: fn(issue, _, _, _, _, emit_update, _, _) {
      process.send(log_subject, "agent_run")
      emit_update(
        issue.id,
        runner.PiUpdate(
          event: "turn_finished",
          message: Some("hello"),
          raw_json: None,
          turn: Some(1),
          request_id: None,
          method: None,
          pi_session_id: None,
          tokens: domain.zero_token_totals(),
          tool_name: None,
          tool_input: None,
          tool_output: None,
          tool_status: None,
        ),
      )
      Ok(success(
        domain.Issue(..issue, state: "Done"),
        "test/tmp/daemon/workspace",
      ))
    },
    workflow_run_dependencies: fake_workflow_run_dependencies(log_subject),
    cleanup: fn(_, _, _) { Ok(Nil) },
    logger: fn(_, event, _, _) {
      process.send(log_subject, event)
      Ok(Nil)
    },
    now_ms: fn() { 42 },
    send_after: fn(_, delay, _) { daemon.TestTimer(delay) },
    cancel_timer: fn(_) { Nil },
    start_event_hub: fn() { hub.start(10, fn() { 42 }) },
    make_control_token: fn() { Ok("test-token") },
    start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
    stop_control_server: fn(_) { Nil },
  )
}

fn disabled_linear_commands() -> linear.CommandClient {
  linear.CommandClient(fetch_comments: fn(_, _) { Ok([]) }, post_ack: fn(_, _) {
    Ok(Nil)
  })
}

fn fake_workflow_run_dependencies(
  log_subject: process.Subject(String),
) -> workflow_run.Dependencies {
  workflow_run.Dependencies(
    prepare_step: fn(
      issue,
      workflow_id,
      run_id,
      _step_id,
      workspace_ref,
      orchestrator,
      _known,
    ) {
      let run_root =
        orchestrator.effective.workspace.root
        <> "/"
        <> workflow_id
        <> "/"
        <> issue.identifier
        <> "/"
        <> run_id
      Ok(workspace_run.PreparedStepWorkspace(
        workflow_id: workflow_id,
        run_id: run_id,
        run_root: run_root,
        workspace_name: workspace_ref.name,
        path: run_root <> "/" <> workspace_ref.name,
        source_workspace_name: workspace_ref.from,
        source_workspace_path: None,
      ))
    },
    after_step: fn(_, step_id, _, _) {
      process.send(log_subject, "yaml_after:" <> step_id)
    },
    cleanup_run: fn(run_root, _) {
      process.send(log_subject, "yaml_cleanup:" <> run_root)
      Ok(Nil)
    },
    command_step: fn(step_id, _command, _workspace, _timeout, secrets, limits) {
      process.send(log_subject, "yaml_command:" <> step_id)
      step_artifact.from_command_result(
        step_id,
        0,
        "stdout:" <> step_id,
        "",
        False,
        secrets,
        limits,
      )
    },
    agent_step: fn(
      issue,
      _step_id,
      prompt,
      _effective,
      _tracker,
      workspace_path,
      _emit_update,
      _command_ready,
    ) {
      process.send(log_subject, "yaml_agent:" <> prompt)
      Ok(runner.WorkerSuccess(
        final_issue: Some(issue),
        final_classification: runner.FinalTerminal,
        workspace_path: workspace_path,
        tokens: domain.zero_token_totals(),
        turns: 1,
        result: domain.ResultArtifact(
          final_response: Some(prompt),
          truncated: False,
          source: "test",
        ),
      ))
    },
  )
}

fn command_ready_workflow_run_dependencies(
  log_subject: process.Subject(String),
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
  workflow_run.Dependencies(
    ..base,
    agent_step: fn(
      issue,
      _step_id,
      prompt,
      _effective,
      _tracker,
      workspace_path,
      _emit_update,
      command_ready,
    ) {
      let command_subject = process.new_subject()
      command_ready(command_subject)
      process.send(log_subject, "agent_ready")
      case process.receive(command_subject, within: 5000) {
        Ok(worker_command.QueuePrompt(message, reply)) -> {
          process.send(log_subject, "prompt:" <> message)
          process.send(reply, worker_command.Queued(Some("queued")))
          Ok(runner.WorkerSuccess(
            final_issue: Some(issue),
            final_classification: runner.FinalTerminal,
            workspace_path: workspace_path,
            tokens: domain.zero_token_totals(),
            turns: 1,
            result: domain.ResultArtifact(
              final_response: Some(prompt <> ":" <> message),
              truncated: False,
              source: "test",
            ),
          ))
        }
        Ok(other) -> {
          let _ = other
          Error(runner.WorkerFailure(
            reason: error.PiFailed(error.PiProtocolError("unexpected_command")),
            workspace_path: Some(workspace_path),
            tokens: domain.zero_token_totals(),
            final_issue: Some(issue),
          ))
        }
        Error(_) ->
          Error(runner.WorkerFailure(
            reason: error.PiFailed(error.PiProtocolError("command_timeout")),
            workspace_path: Some(workspace_path),
            tokens: domain.zero_token_totals(),
            final_issue: Some(issue),
          ))
      }
    },
  )
}

fn crashing_command_ready_workflow_run_dependencies(
  log_subject: process.Subject(String),
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
  workflow_run.Dependencies(
    ..base,
    agent_step: fn(
      _issue,
      _step_id,
      _prompt,
      _effective,
      _tracker,
      _workspace_path,
      _emit_update,
      command_ready,
    ) {
      let command_subject = process.new_subject()
      command_ready(command_subject)
      process.send(log_subject, "agent_ready")
      panic as "yaml agent crashed"
    },
  )
}

fn fake_triage(subject: process.Subject(String)) -> linear_triage.TriageClient {
  linear_triage.TriageClient(report_invalid_workflow: fn(issue, violation) {
    process.send(
      subject,
      "triage:" <> issue.id <> ":" <> workflow_policy.violation_code(violation),
    )
    Ok(linear_triage.InvalidWorkflowReportNoop)
  })
}

fn prompt_until_queued(
  subject: process.Subject(daemon.Message),
  session_id: String,
  attempts: Int,
) -> command.CommandResult {
  let assert True = attempts > 0
  let assert Ok(result) =
    daemon.apply_operator_command(
      subject,
      command.PromptSession(session_id, "hello from operator"),
      1000,
    )
  case result.status {
    command.Queued -> result
    command.NotFound -> {
      process.sleep(50)
      prompt_until_queued(subject, session_id, attempts - 1)
    }
    _ -> result
  }
}

fn wait_for_event(
  subject: process.Subject(String),
  event: String,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case process.receive(subject, within: 500) {
        Ok(received) ->
          case received == event {
            True -> True
            False -> wait_for_event(subject, event, attempts - 1)
          }
        Error(_) -> False
      }
  }
}

pub fn daemon_skips_invalid_workflow_candidate_and_reports_once_test() {
  let workflow_path =
    write_enforcing_workflow("test/tmp/daemon-invalid-workflow", 1)
  let candidate = issue("issue-id", "ABC-1", "Todo")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let triage_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_triage: fn(_, _) { fake_triage(triage_subject) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert process.receive(triage_subject, within: 1000)
    == Ok("triage:issue-id:missing_workflow_label")
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(snapshot.running) == 0
  assert dict.has_key(snapshot.invalid_workflow_reports, "issue-id")

  process.send(started.data, daemon.PollTick(2))
  assert process.receive(triage_subject, within: 200) == Error(Nil)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_reports_invalid_workflow_candidate_when_slots_are_full_test() {
  let workflow_path =
    write_enforcing_workflow("test/tmp/daemon-invalid-workflow-full-slots", 1)
  let valid_candidate =
    domain.Issue(..issue("valid-id", "ABC-1", "Todo"), labels: [
      "workflow:bugfix",
    ])
  let invalid_candidate = issue("invalid-id", "ABC-2", "Todo")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([invalid_candidate, valid_candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([valid_candidate]) },
    )
  let log_subject = process.new_subject()
  let triage_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_triage: fn(_, _) { fake_triage(triage_subject) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert process.receive(triage_subject, within: 1000)
    == Ok("triage:invalid-id:missing_workflow_label")
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_dispatches_valid_workflow_candidate_test() {
  let workflow_path =
    write_enforcing_workflow("test/tmp/daemon-valid-workflow", 1)
  let candidate =
    domain.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:bugfix",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        Ok([domain.Issue(..candidate, state: "Done")])
      },
    )
  let log_subject = process.new_subject()
  let triage_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_triage: fn(_, _) { fake_triage(triage_subject) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "dispatch_started", 10)
  assert process.receive(triage_subject, within: 100) == Error(Nil)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_yaml_agent_steps_get_concrete_sessions_test() {
  let dir = "test/tmp/daemon-yaml-agent-session"
  let workflow_path = write_yaml_agent_workflow(dir)
  let candidate =
    domain.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "worker_exited", 20)
  let assert Ok(sessions) = hub.list_sessions(event_hub, 1000)
  let session_ids = list.map(sessions, fn(summary) { summary.session_id })
  assert list.contains(session_ids, "ABC-1-42-1")
  assert list.contains(session_ids, "ABC-1-42-1-implement")
  let matching_step_sessions =
    list.filter(sessions, fn(summary) {
      summary.session_id == "ABC-1-42-1-implement"
    })
  let assert [step_session] = matching_step_sessions
  assert session_event.exit_reason(step_session.status) == Some("normal")
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_yaml_operator_prompt_routes_to_agent_step_session_test() {
  let dir = "test/tmp/daemon-yaml-agent-command"
  let workflow_path = write_yaml_agent_workflow(dir)
  let candidate =
    domain.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: command_ready_workflow_run_dependencies(
        log_subject,
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "agent_ready", 20)

  let result = prompt_until_queued(started.data, "ABC-1-42-1-implement", 20)
  assert result.status == command.Queued
  assert wait_for_event(log_subject, "prompt:hello from operator", 20)
  assert wait_for_event(log_subject, "worker_exited", 20)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_yaml_agent_step_crash_cleans_command_route_test() {
  let dir = "test/tmp/daemon-yaml-agent-command-crash"
  let workflow_path = write_yaml_agent_workflow(dir)
  let candidate =
    domain.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: crashing_command_ready_workflow_run_dependencies(
        log_subject,
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "agent_ready", 20)
  assert wait_for_event(log_subject, "worker_exited", 20)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.PromptSession("ABC-1-42-1-implement", "after crash"),
      1000,
    )
  assert result.status == command.NotFound
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_yaml_poll_dispatches_command_workflow_test() {
  let dir = "test/tmp/daemon-yaml"
  let assert Ok(marker) = path.absolute(dir <> "/marker")
  let workflow_path = write_yaml_workflow(dir, marker)
  let candidate =
    domain.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "yaml_command:final_test", 20)
  assert wait_for_event(log_subject, "worker_exited", 20)
  assert simplifile.is_file(marker) != Ok(True)
  let assert Ok(sessions) = hub.list_sessions(event_hub, 1000)
  let session_ids = list.map(sessions, fn(summary) { summary.session_id })
  assert list.contains(session_ids, "ABC-1-42-1-final_test")
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(snapshot.completed, "issue-id")
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_poll_dispatches_fake_worker_routes_update_and_shutdown_test() {
  let workflow_path = write_workflow("test/tmp/daemon-basic", 1)
  let candidate = issue("issue-id", "ABC-1", "Todo")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        Ok([domain.Issue(..candidate, state: "Done")])
      },
    )
  let log_subject = process.new_subject()
  let assert Ok(started) =
    daemon.start(Some(workflow_path), base_dependencies(client, log_subject))

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "tick_started", 10)
  assert wait_for_event(log_subject, "dispatch_started", 10)
  assert wait_for_event(log_subject, "pi_event", 10)
  assert wait_for_event(log_subject, "worker_exited", 10)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_retry_timer_requeues_failed_worker_once_test() {
  let workflow_path = write_workflow("test/tmp/daemon-retry", 1)
  let first = issue("retry-id", "ABC-2", "Todo")
  let second = domain.Issue(..first, title: "retry succeeds")
  let log_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([first]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([second]) },
    )
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      agent_runner: fn(issue: domain.Issue, _, _, _, _, _, _, _) {
        process.send(log_subject, "agent_run")
        case issue.title == "retry succeeds" {
          False ->
            Error(runner.WorkerFailure(
              reason: error.PiFailed(error.PiProtocolError("boom")),
              workspace_path: Some("test/tmp/daemon-retry/workspace"),
              tokens: domain.zero_token_totals(),
              final_issue: None,
            ))
          True ->
            Ok(success(
              domain.Issue(..issue, state: "Done"),
              "test/tmp/daemon-retry/workspace",
            ))
        }
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "retry_scheduled", 20)

  process.send(started.data, daemon.RetryTick("retry-id", 99))
  assert wait_for_event(log_subject, "retry_timer_stale", 10)
  process.send(started.data, daemon.RetryTick("retry-id", 1))
  assert wait_for_event(log_subject, "worker_exited", 20)
  process.send(started.data, daemon.RetryTick("retry-id", 1))
  assert wait_for_event(log_subject, "retry_timer_stale", 10)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
