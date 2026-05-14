import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/config/types as config_types
import scherzo/control/client
import scherzo/control/command
import scherzo/control/file as control_file
import scherzo/error
import scherzo/handoff
import scherzo/orchestrator/core
import scherzo/orchestrator/daemon
import scherzo/orchestrator/state as orchestrator_state
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/reason as session_reason
import scherzo/session/tokens as session_tokens
import scherzo/state/ledger
import scherzo/state/record
import scherzo/tracker
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
import scherzo/workflow_run
import simplifile
import test_async

fn prompt_text(mode: workflow_attempt.AgentPromptMode) -> String {
  case mode {
    workflow_attempt.OriginalPrompt(prompt) -> prompt
    workflow_attempt.StructuredOutputRetryPrompt(prompt) -> prompt
    workflow_attempt.RecoveryPrompt(prompt) -> prompt
  }
}

fn reset_dir(dir: String) -> Nil {
  let _ = simplifile.delete(dir)
  let assert Ok(Nil) = simplifile.create_directory_all(dir)
  Nil
}

fn workflow_text(root: String) -> String {
  workflow_text_with_limits(root, 0, 1, 1)
}

fn workflow_text_with_limits(
  root: String,
  max_concurrent_agents: Int,
  max_retry_attempts: Int,
  max_sessions_per_issue: Int,
) -> String {
  "version: 1
tracker:
  kind: linear
  api_key: test-key
  project_slug: TEST
  active_states: [Todo]
  dispatch_states: [Todo]
  terminal_states: [Done]
workspace:
  root: " <> root <> "
  hooks:
    create: |
      mkdir -p \"$SCHERZO_WORKSPACE_PATH\"
    before_step: |
      test -d \"$SCHERZO_WORKSPACE_PATH\"
    after_step: |
      true
    remove: |
      rm -rf \"$SCHERZO_WORKSPACE_PATH\"
    timeout_ms: 60000
polling:
  interval_ms: 1000
agent:
  max_concurrent_agents: " <> int_to_string(max_concurrent_agents) <> "
  max_retry_attempts: " <> int_to_string(max_retry_attempts) <> "
  max_sessions_per_issue: " <> int_to_string(max_sessions_per_issue) <> "
pi:
  command: fake
routing:
  workflow_label_prefix: \"workflow:\"
  require_exactly_one_workflow_label: false
  default_workflow: implementation
  workflows:
    implementation: workflows/implementation.yaml
"
}

fn write_workflow(dir: String) -> #(String, String) {
  reset_dir(dir)
  let root = dir <> "/workspaces"
  #(write_workflow_files(dir, workflow_text(root)), root)
}

fn write_workflow_with_limits(
  dir: String,
  max_concurrent_agents: Int,
  max_retry_attempts: Int,
  max_sessions_per_issue: Int,
) -> #(String, String) {
  reset_dir(dir)
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
    workspace: main
",
    )
  config_path
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

fn dependencies(
  log_subject: process.Subject(String),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..daemon.default_dependencies(),
    make_tracker: fn(_) {
      tracker.Client(
        fetch_candidate_issues: fn() { Ok([]) },
        fetch_issues_by_states: fn(_) { Ok([]) },
        fetch_issue_states_by_ids: fn(_) { Ok([]) },
      )
    },
    make_handoff: fn(_, _) { handoff.disabled_client() },
    cleanup: fn(_, _, _) { Ok(Nil) },
    logger: fn(_, event, fields, _) {
      process.send(log_subject, control_log_value(event, fields))
      Ok(Nil)
    },
    now_ms: fn() { 42 },
    send_after: fn(_, delay, _) { daemon.TestTimer(delay) },
    cancel_timer: fn(_) { Nil },
  )
}

fn dependencies_with_tracker(
  log_subject: process.Subject(String),
  tracker_client: tracker.Client,
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(..dependencies(log_subject), make_tracker: fn(_) {
    tracker_client
  })
}

fn in_process_dependencies(
  log_subject: process.Subject(String),
  tracker_client: tracker.Client,
  handoff_client: handoff.Client,
  hub_subject: process.Subject(hub.Message),
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
    ..dependencies_with_tracker(log_subject, tracker_client),
    make_handoff: fn(_, _) { handoff_client },
    workflow_run_dependencies: workflow_deps_from_agent(agent_runner),
    start_event_hub: fn() { Ok(hub_subject) },
    make_control_token: fn() { Ok("test-token") },
    start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
    stop_control_server: fn(_) { Nil },
  )
}

fn workflow_deps_from_agent(
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
) -> workflow_run.Dependencies {
  workflow_run.Dependencies(
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
  )
}

fn disabled_handoff() -> handoff.Client {
  handoff.disabled_client()
}

fn park_reporting_handoff(subject: process.Subject(String)) -> handoff.Client {
  handoff.Client(
    claim_issue: fn(_, _) { Ok(Nil) },
    report_success: fn(_, _, _) { Ok(Nil) },
    report_success_for_workflow: fn(_, _, _, _) { Ok(Nil) },
    report_failure: fn(_, _, _) { Ok(Nil) },
    report_failure_for_workflow: fn(_, _, _, _) { Ok(Nil) },
    report_park: fn(report) {
      process.send(subject, park_report_message(report))
      Ok(Nil)
    },
  )
}

fn park_report_message(report: handoff.ParkReport) -> String {
  "park:"
  <> report.issue_id
  <> ":"
  <> report.issue_identifier
  <> ":"
  <> report.reason
  <> ":"
  <> option_string(report.release_policy)
  <> ":"
  <> option_string(report.run_id)
}

fn option_string(value: Option(String)) -> String {
  case value {
    None -> ""
    Some(value) -> value
  }
}

fn blocking_handoff(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> handoff.Client {
  handoff.Client(
    claim_issue: fn(_, _) {
      process.send(log_subject, "claim_started")
      test_async.block_until_released(barrier)
      Ok(Nil)
    },
    report_success: fn(_, _, _) { Ok(Nil) },
    report_success_for_workflow: fn(_, _, _, _) { Ok(Nil) },
    report_failure: fn(_, _, _) { Ok(Nil) },
    report_failure_for_workflow: fn(_, _, _, _) { Ok(Nil) },
    report_park: fn(_) { Ok(Nil) },
  )
}

fn long_running_agent(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> fn(
  tracker_issue.Issue,
  Option(Int),
  String,
  config_types.EffectiveConfig,
  tracker.Client,
  fn(String, agent_types.RunnerUpdate) -> Nil,
  process.Subject(worker_command.Command),
  fn() -> Nil,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  fn(issue: tracker_issue.Issue, _, _, _, _, _, _, _) {
    process.send(log_subject, "agent_run:" <> issue.id)
    test_async.block_until_released(barrier)
    Error(agent_types.WorkerFailure(
      reason: error.PiFailed(error.PiProtocolError("stopped")),
      workspace_path: None,
      tokens: session_tokens.zero_token_totals(),
      final_issue: None,
    ))
  }
}

fn failing_agent(
  log_subject: process.Subject(String),
) -> fn(
  tracker_issue.Issue,
  Option(Int),
  String,
  config_types.EffectiveConfig,
  tracker.Client,
  fn(String, agent_types.RunnerUpdate) -> Nil,
  process.Subject(worker_command.Command),
  fn() -> Nil,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  fn(issue: tracker_issue.Issue, _, _, _, _, _, _, _) {
    process.send(log_subject, "agent_run:" <> issue.id)
    Error(agent_types.WorkerFailure(
      reason: error.PiFailed(error.PiProtocolError("boom")),
      workspace_path: Some("test/tmp/failed-workspace"),
      tokens: session_tokens.zero_token_totals(),
      final_issue: None,
    ))
  }
}

fn fail_original_then_block_agent(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> fn(
  tracker_issue.Issue,
  Option(Int),
  String,
  config_types.EffectiveConfig,
  tracker.Client,
  fn(String, agent_types.RunnerUpdate) -> Nil,
  process.Subject(worker_command.Command),
  fn() -> Nil,
) -> Result(agent_types.WorkerSuccess, agent_types.WorkerFailure) {
  fn(issue: tracker_issue.Issue, _, _, _, _, _, _, _) {
    process.send(log_subject, "agent_run:" <> issue.title)
    case issue.title == "Changed title" {
      True -> test_async.block_until_released(barrier)
      False -> Nil
    }
    Error(agent_types.WorkerFailure(
      reason: error.PiFailed(error.PiProtocolError("boom")),
      workspace_path: Some("test/tmp/failed-workspace"),
      tokens: session_tokens.zero_token_totals(),
      final_issue: None,
    ))
  }
}

fn control_log_value(event: String, fields: List(#(String, String))) -> String {
  case event == "control_server_started" {
    True -> find_field(fields, "control_file")
    False -> event
  }
}

fn find_field(fields: List(#(String, String)), key: String) -> String {
  case fields {
    [] -> ""
    [#(field_key, value), ..rest] ->
      case field_key == key {
        True -> value
        False -> find_field(rest, key)
      }
  }
}

pub fn daemon_writes_control_file_and_serves_session_list_test() {
  let #(workflow_path, _root) = write_workflow("test/tmp/daemon-control-basic")
  let log_subject = process.new_subject()
  let assert Ok(started) =
    daemon.start(Some(workflow_path), dependencies(log_subject))
  let assert Ok(path) = process.receive(log_subject, within: 1000)
  let assert Ok(control) = control_file.read(path)
  assert control.host == "127.0.0.1"
  assert control.port > 0
  assert control.token != ""
  let assert Ok([]) = client.list_sessions(control)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_control_server_routes_authenticated_command_to_actor_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-control-command")
  let log_subject = process.new_subject()
  let assert Ok(started) =
    daemon.start(Some(workflow_path), dependencies(log_subject))
  let assert Ok(path) = process.receive(log_subject, within: 1000)
  let assert Ok(control) = control_file.read(path)

  let assert Ok(result) = client.apply_command(control, command.PauseDispatch)
  assert result.command == "pause"
  assert command.status_to_string(result.status) == "applied"

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_park_and_unpark_commands_mutate_runtime_state_test() {
  let candidate = issue("issue-1", "ABC-1", "Todo")
  let tracker_client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let #(workflow_path, _root) = write_workflow("test/tmp/daemon-control-park")
  let log_subject = process.new_subject()
  let park_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..dependencies_with_tracker(log_subject, tracker_client),
      make_handoff: fn(_, _) { park_reporting_handoff(park_subject) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(path) = process.receive(log_subject, within: 1000)
  let assert Ok(control) = control_file.read(path)

  let assert Ok(parked) =
    client.apply_command(
      control,
      command.ParkIssue(command.IssueIdentifier("ABC-1"), "manual"),
    )
  assert command.status_to_string(parked.status) == "applied"
  let assert Ok(snapshot_after_park) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(snapshot_after_park.parked, "issue-1")
  let assert Ok(parked_entry) = dict.get(snapshot_after_park.parked, "issue-1")
  assert parked_entry.release_policy == orchestrator_state.ExplicitUnparkOnly
  assert process.receive(park_subject, within: 1000)
    == Ok("park:issue-1:ABC-1:manual:explicit_unpark_only:")

  let assert Ok(unparked) =
    client.apply_command(
      control,
      command.UnparkIssue(command.IssueIdentifier("ABC-1")),
    )
  assert command.status_to_string(unparked.status) == "applied"
  let assert Ok(snapshot_after_unpark) = daemon.get_snapshot(started.data, 1000)
  assert !dict.has_key(snapshot_after_unpark.parked, "issue-1")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn pause_command_suppresses_dispatch_and_resume_allows_it_test() {
  let candidate = issue("pause-issue", "ABC-PAUSE", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-pause", 1, 3, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      long_running_agent(log_subject, worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(paused) =
    daemon.apply_operator_command(started.data, command.PauseDispatch, 1000)
  assert command.status_to_string(paused.status) == "applied"
  let _ = test_async.drain_subject(log_subject)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "tick_started", 10)
  let assert Ok(paused_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(paused_snapshot.running) == 0
  let paused_logs = test_async.drain_subject(log_subject)
  assert !list.contains(paused_logs, "dispatch_started")

  let assert Ok(resumed) =
    daemon.apply_operator_command(started.data, command.ResumeDispatch, 1000)
  assert command.status_to_string(resumed.status) == "applied"
  process.send(started.data, daemon.PollTick(2))
  assert wait_for_log(log_subject, "dispatch_started", 20)

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_command_rejects_paused_and_dispatches_eligible_issue_test() {
  let candidate = issue("retry-issue", "ABC-RETRY", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-retry", 1, 3, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      long_running_agent(log_subject, worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(_) =
    daemon.apply_operator_command(started.data, command.PauseDispatch, 1000)
  let assert Ok(paused_retry) =
    daemon.apply_operator_command(
      started.data,
      command.RetryIssue(command.IssueId("retry-issue")),
      1000,
    )
  assert command.status_to_string(paused_retry.status) == "rejected"

  let assert Ok(_) =
    daemon.apply_operator_command(started.data, command.ResumeDispatch, 1000)
  let assert Ok(dispatched_retry) =
    daemon.apply_operator_command(
      started.data,
      command.RetryIssue(command.IssueId("retry-issue")),
      1000,
    )
  assert command.status_to_string(dispatched_retry.status) == "applied"
  assert wait_for_log(log_subject, "dispatch_started", 20)

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn retry_rejects_active_pending_and_claimed_issues_test() {
  let active = issue("active-issue", "ABC-ACTIVE", "Todo")
  let tracker_client = tracker_with(active)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-retry-active", 1, 3, 3)
  let log_subject = process.new_subject()
  let active_worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      long_running_agent(log_subject, active_worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "dispatch_started", 20)

  let assert Ok(active_retry) =
    daemon.apply_operator_command(
      started.data,
      command.RetryIssue(command.IssueId("active-issue")),
      1000,
    )
  assert command.status_to_string(active_retry.status) == "rejected"
  test_async.release_barrier(active_worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)

  let pending = issue("pending-issue", "ABC-PENDING", "Todo")
  let tracker_client = tracker_with(pending)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-retry-pending", 1, 3, 3)
  let log_subject = process.new_subject()
  let claim_barrier = test_async.new_barrier()
  let pending_worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      blocking_handoff(log_subject, claim_barrier),
      hub_subject,
      long_running_agent(log_subject, pending_worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "claim_started", 20)

  let assert Ok(pending_retry) =
    daemon.apply_operator_command(
      started.data,
      command.RetryIssue(command.IssueId("pending-issue")),
      1000,
    )
  assert command.status_to_string(pending_retry.status) == "rejected"
  test_async.release_barrier(claim_barrier)
  test_async.release_barrier(pending_worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)

  let claimed = issue("claimed-issue", "ABC-CLAIMED", "Todo")
  let tracker_client = tracker_with(claimed)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-retry-claimed", 1, 3, 3)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      failing_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "retry_scheduled", 20)
  let assert Ok(claimed_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(claimed_snapshot.claimed, "claimed-issue")
  assert !dict.has_key(claimed_snapshot.running, "claimed-issue")

  let assert Ok(claimed_retry) =
    daemon.apply_operator_command(
      started.data,
      command.RetryIssue(command.IssueId("claimed-issue")),
      1000,
    )
  assert command.status_to_string(claimed_retry.status) == "rejected"
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn park_rejects_claimed_issues_test() {
  let candidate = issue("park-claimed", "ABC-PARK-CLAIMED", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-park-claimed", 1, 3, 3)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      failing_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "retry_scheduled", 20)

  let assert Ok(parked) =
    daemon.apply_operator_command(
      started.data,
      command.ParkIssue(command.IssueId("park-claimed"), "manual"),
      1000,
    )
  assert command.status_to_string(parked.status) == "rejected"
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(snapshot.claimed, "park-claimed")
  assert !dict.has_key(snapshot.parked, "park-claimed")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_candidate_dispatch_clears_stale_auto_park_test() {
  let candidate = issue("auto-park", "ABC-AUTO", "Todo")
  let changed = tracker_issue.Issue(..candidate, title: "Changed title")
  let tracker_server = start_control_tracker_server(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-auto-park", 1, 1, 3)
  let log_subject = process.new_subject()
  let park_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      dynamic_control_tracker(tracker_server),
      park_reporting_handoff(park_subject),
      hub_subject,
      fail_original_then_block_agent(log_subject, worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "agent_run:Title ABC-AUTO", 20)
  assert wait_for_log(log_subject, "issue_parked", 20)
  let assert Ok(parked_snapshot) = daemon.get_snapshot(started.data, 1000)
  let assert Ok(parked_entry) = dict.get(parked_snapshot.parked, "auto-park")
  assert parked_entry.release_policy
    == orchestrator_state.AutoUnparkOnIssueChange(core.issue_fingerprint(
      candidate,
    ))
  assert process.receive(park_subject, within: 1000)
    == Ok(
      "park:auto-park:ABC-AUTO:max_retry_attempts:auto_unpark_on_issue_change:ABC-AUTO-42-1",
    )
  let _ = test_async.drain_subject(log_subject)

  process.send(tracker_server, SetControlTrackerCandidate(changed))
  process.send(started.data, daemon.PollTick(2))
  assert wait_for_log(log_subject, "dispatch_started", 20)
  let assert Ok(running_snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(running_snapshot.running, "auto-park")
  assert !dict.has_key(running_snapshot.parked, "auto-park")
  assert !dict.has_key(running_snapshot.retry_attempts, "auto-park")

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn startup_recovery_of_parked_issue_does_not_repost_park_comment_test() {
  let candidate = issue("recovered-park", "ABC-RECOVER", "Todo")
  let dir = "test/tmp/daemon-control-park-recovery"
  let #(workflow_path, root) = write_workflow_with_limits(dir, 1, 3, 3)
  let assert Ok(ledger_path) =
    ledger.path_for_workspace_root(dir <> "/" <> root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.new(
          1,
          1,
          record.IssueParkedV2(
            "recovered-park",
            "ABC-RECOVER",
            "operator_hold",
            "explicit_unpark_only",
            "",
            1,
          ),
        ),
      ],
      True,
    )
  let log_subject = process.new_subject()
  let park_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_with(candidate),
      park_reporting_handoff(park_subject),
      hub_subject,
      failing_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(snapshot.parked, "recovered-park")
  assert process.receive(park_subject, within: 100) == Error(Nil)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn startup_recovery_new_park_posts_park_comment_test() {
  let candidate = issue("recovery-new-park", "ABC-NEWREC", "Todo")
  let dir = "test/tmp/daemon-control-park-recovery-new"
  let #(workflow_path, root) = write_workflow_with_limits(dir, 1, 1, 3)
  let assert Ok(ledger_path) =
    ledger.path_for_workspace_root(dir <> "/" <> root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.new(
          1,
          1,
          record.RunStarted(
            "ABC-NEWREC-42-1",
            "recovery-new-park",
            "ABC-NEWREC",
            "test/tmp/recovery-new-park-workspace",
          ),
        ),
      ],
      True,
    )
  let log_subject = process.new_subject()
  let park_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_with(candidate),
      park_reporting_handoff(park_subject),
      hub_subject,
      failing_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(snapshot.parked, "recovery-new-park")
  assert process.receive(park_subject, within: 1000)
    == Ok(
      "park:recovery-new-park:ABC-NEWREC:max_retry_attempts:auto_unpark_on_issue_change:ABC-NEWREC-42-1",
    )

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn reload_workflow_command_reports_success_and_missing_file_failure_test() {
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-reload", 0, 1, 1)
  let log_subject = process.new_subject()
  let assert Ok(hub_subject) = hub.start(10, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      empty_tracker(),
      disabled_handoff(),
      hub_subject,
      failing_agent(log_subject),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(reloaded) =
    daemon.apply_operator_command(started.data, command.ReloadWorkflow, 1000)
  assert command.status_to_string(reloaded.status) == "applied"

  let assert Ok(Nil) = simplifile.delete(workflow_path)
  let assert Ok(missing) =
    daemon.apply_operator_command(started.data, command.ReloadWorkflow, 1000)
  assert command.status_to_string(missing.status) == "rejected"

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn abort_command_falls_back_to_kill_and_park_when_worker_does_not_reply_test() {
  assert_session_stop_command(
    "test/tmp/daemon-control-abort",
    issue("abort-issue", "ABC-ABORT", "Todo"),
    command.AbortSession("ABC-ABORT-42-1"),
    session_reason.OperatorAbort,
  )
}

pub fn prompt_command_reaches_live_worker_command_subject_test() {
  let candidate = issue("prompt-live-issue", "ABC-LIVE", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-live-prompt", 1, 3, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      fn(issue: tracker_issue.Issue, _, _, _, _, _, command_subject, ready) {
        ready()
        process.send(log_subject, "agent_run:" <> issue.id)
        let assert Ok(worker_command.QueuePrompt(message, reply)) =
          process.receive(command_subject, within: 1000)
        assert message == "status?"
        process.send(reply, worker_command.Applied(Some("prompt accepted")))
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
  assert wait_for_log(log_subject, "agent_run:prompt-live-issue", 20)

  let assert Ok(prompt_result) =
    daemon.apply_operator_command(
      started.data,
      command.PromptSession("ABC-LIVE-42-1", "status?"),
      1000,
    )
  assert command.status_to_string(prompt_result.status) == "applied"
  assert prompt_result.target == Some("ABC-LIVE-42-1")
  assert prompt_result.message == Some("prompt accepted")

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn prompt_and_respond_ui_commands_reject_workers_without_command_subject_test() {
  let candidate = issue("prompt-issue", "ABC-PROMPT", "Todo")
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) =
    write_workflow_with_limits("test/tmp/daemon-control-prompt", 1, 3, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      long_running_agent(log_subject, worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "dispatch_started", 20)
  assert wait_for_log(log_subject, "agent_run:prompt-issue", 20)

  let assert Ok(prompt_result) =
    daemon.apply_operator_command(
      started.data,
      command.PromptSession("ABC-PROMPT-42-1", "status?"),
      1000,
    )
  assert command.status_to_string(prompt_result.status) == "not_allowed"
  assert command.status_reason(prompt_result.status)
    == Some("worker_command_subject_unavailable")

  let assert Ok(ui_result) =
    daemon.apply_operator_command(
      started.data,
      command.RespondUi("ABC-PROMPT-42-1", "ui-1", command.UiCancel),
      1000,
    )
  assert command.status_to_string(ui_result.status) == "not_allowed"
  assert command.status_reason(ui_result.status)
    == Some("worker_command_subject_unavailable")

  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

pub fn daemon_shutdown_closes_control_server_and_removes_control_file_test() {
  let #(workflow_path, _root) =
    write_workflow("test/tmp/daemon-control-shutdown")
  let log_subject = process.new_subject()
  let assert Ok(started) =
    daemon.start(Some(workflow_path), dependencies(log_subject))
  let assert Ok(path) = process.receive(log_subject, within: 1000)
  let assert Ok(control) = control_file.read(path)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  let assert Error(client.ConnectionFailed(_)) = client.ping(control)
  assert simplifile.is_file(path) != Ok(True)
}

fn assert_session_stop_command(
  dir: String,
  candidate: tracker_issue.Issue,
  operator_command: command.OperatorCommand,
  reason: session_reason.WorkerExitReason,
) -> Nil {
  let tracker_client = tracker_with(candidate)
  let #(workflow_path, _root) = write_workflow_with_limits(dir, 1, 3, 3)
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(hub_subject) = hub.start(50, fn() { 42 })
  let deps =
    in_process_dependencies(
      log_subject,
      tracker_client,
      disabled_handoff(),
      hub_subject,
      long_running_agent(log_subject, worker_barrier),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_log(log_subject, "dispatch_started", 20)

  let assert Ok(result) =
    daemon.apply_operator_command(started.data, operator_command, 1000)
  assert command.status_to_string(result.status) == "applied"
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert !dict.has_key(snapshot.running, candidate.id)
  assert dict.has_key(snapshot.parked, candidate.id)

  let assert Ok(summary) =
    wait_for_session_exit(hub_subject, candidate.identifier <> "-42-1", 20)
  assert summary.status == event.Exited(reason)

  test_async.release_barrier_if_waiting(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(hub_subject)
}

fn tracker_with(candidate: tracker_issue.Issue) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([candidate]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
  )
}

type ControlTrackerMessage {
  SetControlTrackerCandidate(tracker_issue.Issue)
  FetchControlTrackerCandidates(
    process.Subject(Result(List(tracker_issue.Issue), error.TrackerError)),
  )
  FetchControlTrackerByIds(
    List(String),
    process.Subject(Result(List(tracker_issue.Issue), error.TrackerError)),
  )
}

fn start_control_tracker_server(
  candidate: tracker_issue.Issue,
) -> process.Subject(ControlTrackerMessage) {
  let ready = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      control_tracker_loop(subject, candidate)
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
}

fn control_tracker_loop(
  subject: process.Subject(ControlTrackerMessage),
  candidate: tracker_issue.Issue,
) -> Nil {
  case process.receive(subject, within: 10_000) {
    Ok(SetControlTrackerCandidate(candidate)) ->
      control_tracker_loop(subject, candidate)
    Ok(FetchControlTrackerCandidates(reply)) -> {
      process.send(reply, Ok([candidate]))
      control_tracker_loop(subject, candidate)
    }
    Ok(FetchControlTrackerByIds(ids, reply)) -> {
      case list.contains(ids, candidate.id) {
        True -> process.send(reply, Ok([candidate]))
        False -> process.send(reply, Ok([]))
      }
      control_tracker_loop(subject, candidate)
    }
    Error(_) -> Nil
  }
}

fn dynamic_control_tracker(
  server: process.Subject(ControlTrackerMessage),
) -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() {
      let reply = process.new_subject()
      process.send(server, FetchControlTrackerCandidates(reply))
      case process.receive(reply, within: 1000) {
        Ok(result) -> result
        Error(_) -> Ok([])
      }
    },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(ids) {
      let reply = process.new_subject()
      process.send(server, FetchControlTrackerByIds(ids, reply))
      case process.receive(reply, within: 1000) {
        Ok(result) -> result
        Error(_) -> Ok([])
      }
    },
  )
}

fn empty_tracker() -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([]) },
  )
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

fn wait_for_session_exit(
  subject: process.Subject(hub.Message),
  session_id: String,
  attempts: Int,
) -> Result(event.SessionSummary, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case hub.get_session(subject, session_id, 100) {
        Ok(Some(summary)) ->
          case summary.status {
            event.Exited(_) -> Ok(summary)
            _ -> wait_for_session_exit(subject, session_id, attempts - 1)
          }
        _ -> wait_for_session_exit(subject, session_id, attempts - 1)
      }
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
