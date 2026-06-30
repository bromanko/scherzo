import birl
import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/agent/pi_event
import scherzo/agent/types as agent_types
import scherzo/agent/worker_command
import scherzo/control/command
import scherzo/control/query/types as query_types
import scherzo/error
import scherzo/handoff_format
import scherzo/orchestrator/core
import scherzo/orchestrator/daemon
import scherzo/orchestrator/daemon_transition_shell
import scherzo/orchestrator/poll_jitter
import scherzo/orchestrator/scheduled_runtime
import scherzo/orchestrator/transition_invariants
import scherzo/path
import scherzo/result_artifact
import scherzo/runtime/state as orchestrator_state
import scherzo/runtime_bundle
import scherzo/scheduled_failure_reporter
import scherzo/session/event
import scherzo/session/hub
import scherzo/session/reason
import scherzo/session/tokens as session_tokens
import scherzo/state/artifact_store
import scherzo/state/ledger
import scherzo/state/outbox
import scherzo/state/projection
import scherzo/state/record
import scherzo/step_artifact
import scherzo/task
import scherzo/tracker
import scherzo/tracker/adapter
import scherzo/tracker/adapter_legacy
import scherzo/tracker/issue as tracker_issue
import scherzo/tracker/state as issue_state
import scherzo/workflow_attempt
import scherzo/workflow_checkpoint
import scherzo/workflow_fingerprint
import scherzo/workflow_run
import scherzo/workspace
import scherzo/workspace_run
import simplifile
import support/expected_crash
import support/test_helpers
import test_async

fn workspace_source(
  from: Option(String),
  run_root: String,
) -> workspace.WorkspaceSource {
  case from {
    None -> workspace.FreshWorkspace
    Some(name) -> workspace.DerivedWorkspace(name, run_root <> "/" <> name)
  }
}

fn prompt_text(mode: workflow_attempt.AgentPromptMode) -> String {
  case mode {
    workflow_attempt.OriginalPrompt(prompt) -> prompt
    workflow_attempt.StructuredOutputRetryPrompt(prompt) -> prompt
    workflow_attempt.StepRecoveryPrompt(prompt) -> prompt
    workflow_attempt.RecoveryPrompt(prompt) -> prompt
  }
}

fn ms(iso: String) -> Int {
  let assert Ok(time) = birl.parse(iso)
  birl.to_unix_milli(time)
}

fn issue(id: String, identifier: String, state: String) -> tracker_issue.Issue {
  tracker_issue.Issue(
    id: id,
    identifier: identifier,
    title: "Title " <> identifier,
    description: None,
    priority: Some(1),
    state: issue_state.from_string_unchecked(state),
    branch_name: None,
    url: None,
    labels: [],
    blocked_by: [],
    blocked_by_complete: True,
    created_at: Some(birl.from_unix(0)),
    updated_at: Some(birl.from_unix(0)),
  )
}

fn empty_tracker_client() -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([]) },
  )
}

fn workflow_text(root: String, max_concurrent: Int) -> String {
  workflow_text_with_label_policy(root, max_concurrent, False, False)
}

fn workflow_text_with_label_policy(
  root: String,
  max_concurrent: Int,
  require_exactly_one: Bool,
  comment_on_invalid: Bool,
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
  polling:
    every: 1s
workspace:
  root: " <> root <> "
agents:
  concurrency: " <> int_to_string(max_concurrent) <> "
  sessions_per_task: 2
  runtime:
    type: pi
    pi:
      executable: fake
task_routing:
  labels:
    require_exactly_one: " <> bool_to_yaml(require_exactly_one) <> "
    default_workflow: implementation
    on_invalid:
      comment: " <> bool_to_yaml(comment_on_invalid) <> "
workflows:
  implementation: workflows/implementation.yaml
"
}

fn bool_to_yaml(value: Bool) -> String {
  case value {
    True -> "true"
    False -> "false"
  }
}

fn write_workflow(dir: String, max_concurrent: Int) -> String {
  test_helpers.reset_dir(dir)
  write_workflow_files(dir, workflow_text(dir <> "/workspaces", max_concurrent))
}

fn write_workflow_with_absolute_root(
  dir: String,
  max_concurrent: Int,
) -> String {
  test_helpers.reset_dir(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  write_workflow_files(dir, workflow_text(root, max_concurrent))
}

fn write_enforcing_workflow(dir: String, max_concurrent: Int) -> String {
  write_workflow_with_label_policy(dir, max_concurrent, True, True)
}

fn write_enforcing_split_state_workflow(
  dir: String,
  max_concurrent: Int,
) -> String {
  test_helpers.reset_dir(dir)
  let config_text =
    workflow_text_with_label_policy(
      dir <> "/workspaces",
      max_concurrent,
      True,
      True,
    )
    |> string.replace(
      each: "    active: [Todo]",
      with: "    active: [Todo, In Progress]",
    )
  write_workflow_files(dir, config_text)
}

fn write_yaml_agent_workflow(dir: String) -> String {
  test_helpers.reset_dir(dir)
  write_workflow_files(dir, workflow_text(dir <> "/workspaces", 1))
}

fn write_parallel_yaml_agent_workflow(dir: String) -> String {
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) =
    simplifile.write(config_path, workflow_text(dir <> "/workspaces", 1))
  let assert Ok(Nil) = simplifile.write(prompt_dir <> "/task.md", "Prompt")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
concurrency: 2
steps:
  - id: alpha
    kind: agent
    prompt: prompts/task.md
    run_in: alpha
  - id: beta
    kind: agent
    prompt: prompts/task.md
    run_in: beta
  - id: final
    kind: command
    depends_on: [alpha, beta]
    run: final
    run_in: alpha
",
    )
  config_path
}

fn write_yaml_workflow(dir: String, _marker: String) -> String {
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let root = dir <> "/workspaces"
  let assert Ok(Nil) = simplifile.write(config_path, workflow_text(root, 1))
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: final_test
    kind: command
    run: sh -c 'exit 1'
    run_in: main
",
    )
  config_path
}

fn write_scheduled_reporting_workflow(dir: String) -> String {
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      workflow_text(root, 1)
        <> "schedules:\n  - id: scheduled-job\n    workflow: implementation\n    enabled: true\n    every: 1s\n    overlap: skip\n    catch_up: false\n    on_failure:\n      task:\n        enabled: true\n        state: Triage\n        labels:\n          - job:scheduled-job\n        dedupe: open_task_per_schedule\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: scheduled_command
    kind: command
    run: exit 1
    run_in: main
",
    )
  config_path
}

fn write_scheduled_command_workflow(
  dir: String,
  max_concurrent: Int,
) -> String {
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      workflow_text(root, max_concurrent)
        <> "schedules:\n  - id: scheduled-job\n    workflow: implementation\n    enabled: true\n    every: 1s\n    overlap: skip\n    catch_up: false\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: scheduled_command
    kind: command
    run: echo scheduled
    run_in: main
",
    )
  config_path
}

fn write_scheduled_parallel_failure_workflow(dir: String) -> String {
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      workflow_text(root, 1)
        <> "schedules:\n  - id: scheduled-job\n    workflow: implementation\n    enabled: true\n    every: 1s\n    overlap: skip\n    catch_up: false\n",
    )
  let assert Ok(Nil) = simplifile.write(prompt_dir <> "/task.md", "Prompt")
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
concurrency: 2
steps:
  - id: active_agent
    kind: agent
    prompt: prompts/task.md
    run_in: alpha
  - id: failing_command
    kind: command
    run: exit 1
    run_in: beta
  - id: terminal_join
    kind: command
    depends_on: [active_agent, failing_command]
    run: echo joined
    run_in: main
",
    )
  config_path
}

fn write_scheduled_capacity_workflow(
  dir: String,
  max_concurrent: Int,
) -> String {
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      workflow_text(root, max_concurrent)
        <> "schedules:\n  - id: capacity-a\n    workflow: implementation\n    enabled: true\n    every: 1s\n    overlap: skip\n    catch_up: false\n  - id: capacity-b\n    workflow: implementation\n    enabled: true\n    every: 1s\n    overlap: skip\n    catch_up: false\n  - id: capacity-c\n    workflow: implementation\n    enabled: true\n    every: 1s\n    overlap: skip\n    catch_up: false\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: capacity_command
    kind: command
    run: echo capacity
    run_in: main
",
    )
  config_path
}

fn write_real_failing_command_workflow(dir: String) -> String {
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let root = dir <> "/workspaces"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(Nil) =
    simplifile.write(
      config_path,
      workflow_text(root, 1)
        <> "artifacts:\n  limits:\n    command_output_chars: 40\n    template_field_chars: 200\n    workflow_summary_chars: 200\n",
    )
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: final_test
    kind: command
    run: |
      touch ../.scherzo-keep-workspace
      printf 'stdout-abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz\\n'
      printf 'stderr-abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz\\n' >&2
      exit 9
    run_in: main
",
    )
  config_path
}

fn write_workflow_with_label_policy(
  dir: String,
  max_concurrent: Int,
  require_exactly_one: Bool,
  comment_on_invalid: Bool,
) -> String {
  test_helpers.reset_dir(dir)
  write_workflow_files(
    dir,
    workflow_text_with_label_policy(
      dir <> "/workspaces",
      max_concurrent,
      require_exactly_one,
      comment_on_invalid,
    ),
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

fn base_dependencies(
  client: tracker.Client,
  log_subject: process.Subject(String),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..daemon.default_dependencies(),
    make_tracker_adapter: fn(_) { legacy_adapter(client) },
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

fn wait_until_startup_recovery_ready(
  daemon_subject: process.Subject(daemon.Message),
) -> Nil {
  let assert Ok(Nil) = daemon.await_startup_recovery_ready(daemon_subject, 1000)
  Nil
}

type InvariantCheckCommand {
  CheckInvariants(
    process.Subject(Result(Nil, List(transition_invariants.InvariantError))),
  )
  StopInvariantChecker
}

fn scripted_invariant_checker(
  fail_after_successes: Int,
) -> #(
  daemon_transition_shell.InvariantChecker,
  process.Subject(InvariantCheckCommand),
) {
  let ready = process.new_subject()
  let _pid =
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      scripted_invariant_checker_loop(subject, fail_after_successes, 0)
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  #(
    fn(_) {
      let reply = process.new_subject()
      process.send(subject, CheckInvariants(reply))
      case process.receive(reply, within: 1000) {
        Ok(result) -> result
        Error(Nil) -> Ok(Nil)
      }
    },
    subject,
  )
}

fn scripted_invariant_checker_loop(
  subject: process.Subject(InvariantCheckCommand),
  fail_after_successes: Int,
  success_count: Int,
) -> Nil {
  case process.receive(subject, within: 60_000) {
    Ok(StopInvariantChecker) -> Nil
    Ok(CheckInvariants(reply)) -> {
      let should_fail = success_count >= fail_after_successes
      case should_fail {
        True -> {
          process.send(reply, Error([test_transition_invariant_error()]))
          scripted_invariant_checker_loop(
            subject,
            fail_after_successes,
            success_count,
          )
        }
        False -> {
          process.send(reply, Ok(Nil))
          scripted_invariant_checker_loop(
            subject,
            fail_after_successes,
            success_count + 1,
          )
        }
      }
    }
    Error(Nil) -> Nil
  }
}

fn test_transition_invariant_error() -> transition_invariants.InvariantError {
  transition_invariants.InvariantError(
    "test_fatal_invariant",
    "test-identity",
    "test violation",
  )
}

pub fn daemon_startup_invariant_failure_aborts_and_cleans_up_test() {
  use <- expected_crash.suppressing(["transition_invariant_violation"])
  let workflow_path =
    write_workflow_with_absolute_root("test/tmp/daemon-startup-invariant", 1)
  let client = empty_tracker_client()
  let log_subject = process.new_subject()
  let cleanup_subject = process.new_subject()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let assert Ok(event_hub_pid) = process.subject_owner(event_hub)
  let event_hub_monitor = process.monitor(event_hub_pid)
  let #(checker, checker_subject) = scripted_invariant_checker(0)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      start_event_hub: fn() { Ok(event_hub) },
      stop_control_server: fn(_) {
        process.send(cleanup_subject, "control_stop")
      },
      check_transition_invariants: checker,
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(daemon_pid) = process.subject_owner(started.data)
  process.unlink(daemon_pid)
  let daemon_monitor = process.monitor(daemon_pid)

  assert wait_for_event(log_subject, "transition_invariant_violation", 10)
  let daemon_stopped = wait_for_monitor_down(daemon_monitor, 1000)
  let event_hub_stopped = wait_for_monitor_down(event_hub_monitor, 1000)
  process.send(checker_subject, StopInvariantChecker)
  case daemon_stopped {
    True -> Nil
    False -> process.kill(daemon_pid)
  }
  case event_hub_stopped {
    True -> Nil
    False -> hub.stop(event_hub)
  }
  process.demonitor_process(daemon_monitor)
  process.demonitor_process(event_hub_monitor)
  assert process.receive(cleanup_subject, within: 1000) == Ok("control_stop")
  assert daemon_stopped
  assert event_hub_stopped
}

pub fn daemon_start_maps_actor_init_timeout_to_specific_startup_error_test() {
  let workflow_path =
    write_workflow_with_absolute_root("test/tmp/daemon-startup-timeout", 1)
  let barrier = test_async.new_barrier()
  let log_fields_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(empty_tracker_client(), process.new_subject()),
      logger: fn(_, event, fields, _) {
        process.send(log_fields_subject, #(event, fields))
        Ok(Nil)
      },
      start_control_server: fn(_, _) {
        test_async.block_until_released(barrier)
        Ok(daemon.NoControlServer)
      },
    )

  let result =
    daemon.start_with_initialiser_timeout(Some(workflow_path), deps, 10)
  test_async.release_barrier_if_waiting(barrier)

  let assert Error(daemon.StartupError(code, message)) = result
  assert code == "daemon_actor_init_timeout"
  assert string.contains(message, "control_plane_starting")

  let assert Ok(fields) =
    wait_for_log_fields(log_fields_subject, "daemon_startup_timeout", 20)
  let field_map = dict.from_list(fields)
  assert dict.get(field_map, "initialiser_timeout_ms") == Ok("10")
  assert dict.get(field_map, "last_startup_phase")
    == Ok("control_plane_starting")
}

pub fn daemon_start_returns_before_post_init_recovery_completes_test() {
  let workflow_path =
    write_workflow_with_absolute_root("test/tmp/daemon-post-init-recovery", 1)
  let recovery_stage_subject = process.new_subject()
  let recovery_continue_subject = process.new_subject()
  let timer_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(empty_tracker_client(), process.new_subject()),
      send_after: fn(_, delay_ms, message) {
        process.send(timer_subject, #(delay_ms, message))
        daemon.TestTimer(delay_ms)
      },
      enqueue_startup_recovery_message: fn(_, message) {
        process.send(recovery_continue_subject, message)
      },
      observe_startup_recovery_stage: fn(stage) {
        process.send(recovery_stage_subject, stage)
      },
    )

  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  let assert Ok(_) = daemon.get_snapshot(started.data, 1000)
  test_async.assert_no_extra_message(timer_subject)

  process.send(
    started.data,
    test_async.expect_message(recovery_continue_subject),
  )
  let assert "startup_recovery" =
    test_async.expect_message(recovery_stage_subject)
  process.send(
    started.data,
    test_async.expect_message(recovery_continue_subject),
  )
  let assert "scheduled_startup_recovery" =
    test_async.expect_message(recovery_stage_subject)
  process.send(
    started.data,
    test_async.expect_message(recovery_continue_subject),
  )
  let assert "workflow_resumptions" =
    test_async.expect_message(recovery_stage_subject)
  process.send(
    started.data,
    test_async.expect_message(recovery_continue_subject),
  )
  let assert "startup_transition_invariants" =
    test_async.expect_message(recovery_stage_subject)
  process.send(
    started.data,
    test_async.expect_message(recovery_continue_subject),
  )
  let assert "startup_recovery_ready" =
    test_async.expect_message(recovery_stage_subject)
  assert test_async.expect_message(timer_subject) == #(0, daemon.PollTick(1))

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_runtime_invariant_failure_stops_after_cleanup_test() {
  use <- expected_crash.suppressing(["transition_invariant_violation"])
  let workflow_path =
    write_workflow_with_absolute_root("test/tmp/daemon-runtime-invariant", 1)
  let client = empty_tracker_client()
  let log_subject = process.new_subject()
  let cleanup_subject = process.new_subject()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let assert Ok(event_hub_pid) = process.subject_owner(event_hub)
  let event_hub_monitor = process.monitor(event_hub_pid)
  let #(checker, checker_subject) = scripted_invariant_checker(2)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      start_event_hub: fn() { Ok(event_hub) },
      stop_control_server: fn(_) {
        process.send(cleanup_subject, "control_stop")
      },
      check_transition_invariants: checker,
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)
  let assert Ok(daemon_pid) = process.subject_owner(started.data)
  process.unlink(daemon_pid)
  let daemon_monitor = process.monitor(daemon_pid)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "transition_invariant_violation", 10)
  let daemon_stopped = wait_for_monitor_down(daemon_monitor, 1000)
  let event_hub_stopped = wait_for_monitor_down(event_hub_monitor, 1000)
  process.send(checker_subject, StopInvariantChecker)
  case daemon_stopped {
    True -> Nil
    False -> process.kill(daemon_pid)
  }
  case event_hub_stopped {
    True -> Nil
    False -> hub.stop(event_hub)
  }
  process.demonitor_process(daemon_monitor)
  process.demonitor_process(event_hub_monitor)
  assert process.receive(cleanup_subject, within: 1000) == Ok("control_stop")
  assert daemon_stopped
  assert event_hub_stopped
}

pub fn daemon_schedules_jittered_recurring_poll_after_immediate_tick_test() {
  let workflow_path = write_workflow("test/tmp/daemon-poll-jitter", 1)
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let log_fields_subject = process.new_subject()
  let timer_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      logger: fn(_, event, fields, _) {
        process.send(log_fields_subject, #(event, fields))
        Ok(Nil)
      },
      send_after: fn(_, delay_ms, message) {
        process.send(timer_subject, #(delay_ms, message))
        daemon.TestTimer(delay_ms)
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  assert process.receive(timer_subject, within: 1000)
    == Ok(#(0, daemon.PollTick(1)))
  process.send(started.data, daemon.PollTick(1))

  let assert Ok(#(delay_ms, daemon.PollTick(2))) =
    process.receive(timer_subject, within: 1000)
  let jitter_bound_ms = poll_jitter.jitter_bound_ms(1000)
  assert delay_ms > 0
  assert delay_ms >= 1000 - jitter_bound_ms
  assert delay_ms <= 1000 + jitter_bound_ms

  let assert Ok(fields) =
    wait_for_log_fields(log_fields_subject, "next_poll_scheduled", 20)
  let field_map = dict.from_list(fields)
  assert dict.get(field_map, "generation") == Ok("2")
  assert dict.get(field_map, "polling_interval_ms") == Ok("1000")
  assert dict.get(field_map, "polling_jitter_bound_ms")
    == Ok(int_to_string(jitter_bound_ms))
  assert dict.get(field_map, "next_poll_delay_ms")
    == Ok(int_to_string(delay_ms))
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_poll_does_not_fetch_remote_command_events_test() {
  let workflow_path =
    write_workflow("test/tmp/daemon-no-remote-command-fetch", 1)
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([issue("issue-1", "ABC-1", "Todo")]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let remote_command_subject = process.new_subject()
  let timer_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_tracker_adapter: fn(_) {
        adapter.TrackerAdapter(
          ..legacy_adapter(client),
          remote_commands: Some(
            adapter.RemoteCommandCapability(
              fetch_events: fn(_) {
                process.send(remote_command_subject, "fetch_events")
                Ok([
                  adapter.RemoteCommandEvent(
                    event_id: "comment-1",
                    task: task.TaskRef(
                      backend_kind: "linear",
                      remote_id: "issue-1",
                      key: Some("ABC-1"),
                      url: None,
                    ),
                    author_id: "lin-user",
                    body: "/scherzo retry",
                    command_name: "retry",
                    excerpt: "/scherzo retry",
                    observed_at_ms: 42,
                  ),
                ])
              },
              post_ack: fn(ack) {
                process.send(remote_command_subject, "post_ack")
                let adapter.RemoteCommandAck(event: event, ..) = ack
                let adapter.RemoteCommandEvent(task: command_task, ..) = event
                Ok(adapter.CommentReceipt(
                  id: "remote-command-ack",
                  task: command_task,
                  url: None,
                  created: True,
                ))
              },
            ),
          ),
        )
      },
      send_after: fn(_, delay_ms, message) {
        process.send(timer_subject, #(delay_ms, message))
        daemon.TestTimer(delay_ms)
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)

  let assert Ok(#(_, daemon.PollTick(generation))) =
    process.receive(timer_subject, within: 1000)
  process.send(started.data, daemon.PollTick(generation))
  let assert Ok(#(_, daemon.PollTick(_))) =
    process.receive(timer_subject, within: 1000)

  test_async.assert_no_extra_message(remote_command_subject)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

fn legacy_adapter(client: tracker.Client) -> adapter.TrackerAdapter {
  adapter_legacy.adapter_from_legacy_client(client, "linear")
}

fn adapter_with_invalid_comment_subject(
  client: tracker.Client,
  subject: process.Subject(String),
) -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    ..legacy_adapter(client),
    comments: Some(
      adapter.CommentCapability(
        post_or_update: fn(request) {
          process.send(
            subject,
            "triage:" <> request.task.remote_id <> ":missing_workflow_label",
          )
          Ok(adapter.CommentReceipt(
            id: "invalid-workflow-comment",
            task: request.task,
            url: None,
            created: True,
          ))
        },
        find_by_marker: fn(_) { Ok(None) },
      ),
    ),
  )
}

fn adapter_with_handoff_failure_subject(
  client: tracker.Client,
  subject: process.Subject(String),
) -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    ..legacy_adapter(client),
    handoff: Some(
      adapter.HandoffCapability(report: fn(event) {
        case event {
          adapter.HandoffFailure(task_context, failure, run_id, _) -> {
            process.send(
              subject,
              handoff_format.failure_comment(
                task.to_runtime_issue(task_context),
                failure,
                run_id,
                [],
              ),
            )
            Ok(Nil)
          }
          adapter.HandoffClaim(_, _, _)
          | adapter.HandoffSuccess(_, _, _, _)
          | adapter.HandoffPark(_) -> Ok(Nil)
        }
      }),
    ),
  )
}

fn adapter_with_handoff_claim_subject(
  client: tracker.Client,
  subject: process.Subject(#(String, String)),
) -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    ..legacy_adapter(client),
    handoff: Some(
      adapter.HandoffCapability(report: fn(event) {
        case event {
          adapter.HandoffClaim(task_context, _, run_id) -> {
            let claimed_issue = task.to_runtime_issue(task_context)
            process.send(subject, #(claimed_issue.id, run_id))
            Ok(Nil)
          }
          adapter.HandoffSuccess(_, _, _, _)
          | adapter.HandoffFailure(_, _, _, _)
          | adapter.HandoffPark(_) -> Ok(Nil)
        }
      }),
    ),
  )
}

fn adapter_with_scheduled_reporter(
  client: tracker.Client,
  reporter: scheduled_failure_reporter.Client,
) -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    ..legacy_adapter(client),
    scheduled_failures: Some(
      adapter.ScheduledFailureCapability(publish: fn(publication) {
        publish_scheduled_failure_for_test(reporter, publication)
      }),
    ),
  )
}

fn adapter_with_park_report_subject(
  client: tracker.Client,
  subject: process.Subject(#(String, String, Option(String))),
) -> adapter.TrackerAdapter {
  adapter.TrackerAdapter(
    ..legacy_adapter(client),
    handoff: Some(
      adapter.HandoffCapability(report: fn(event) {
        case event {
          adapter.HandoffPark(report) -> {
            process.send(subject, #(
              report.task.remote_id,
              report.reason,
              report.run_id,
            ))
            Ok(Nil)
          }
          _ -> Ok(Nil)
        }
      }),
    ),
  )
}

fn publish_scheduled_failure_for_test(
  reporter: scheduled_failure_reporter.Client,
  publication: adapter.ScheduledFailurePublication,
) -> Result(adapter.ScheduledFailureReceipt, adapter.TrackerError) {
  use target_state <- try_adapter_result(required_option(
    publication.target_state_name,
    "scheduled_failures.target_state",
  ))
  use outcome <- try_adapter_tracker(
    reporter.report_failure(scheduled_failure_reporter.FailureReportRequest(
      job_id: publication.job_id,
      workflow_id: publication.workflow_id,
      due_at_ms: publication.due_at_ms,
      run_id: publication.run_id,
      attempt: publication.attempt,
      max_attempts: publication.max_attempts,
      reason: publication.reason,
      run_root: publication.run_root,
      session_id: publication.session_id,
      dedupe_key: publication.dedupe_key,
      triage_state: target_state,
      configured_labels: publication.labels,
      previous_issue_id: publication.previous_task_remote_id,
    )),
  )
  case outcome {
    scheduled_failure_reporter.FailureReportCreated(issue_id) ->
      Ok(scheduled_failure_receipt(issue_id, True))
    scheduled_failure_reporter.FailureReportUpdated(issue_id) ->
      Ok(scheduled_failure_receipt(issue_id, False))
    scheduled_failure_reporter.FailureReportNoop ->
      Error(adapter.UnsupportedCapability("scheduled_failures.publish"))
  }
}

fn scheduled_failure_receipt(
  issue_id: String,
  created: Bool,
) -> adapter.ScheduledFailureReceipt {
  adapter.ScheduledFailureReceipt(
    task: task.TaskRef(
      backend_kind: "linear",
      remote_id: issue_id,
      key: None,
      url: None,
    ),
    created: created,
    comment_id: None,
  )
}

fn required_option(
  value: Option(a),
  capability: String,
) -> Result(a, adapter.TrackerError) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(adapter.UnsupportedCapability(capability))
  }
}

fn try_adapter_result(
  result: Result(a, adapter.TrackerError),
  next: fn(a) -> Result(b, adapter.TrackerError),
) -> Result(b, adapter.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(error) -> Error(error)
  }
}

fn try_adapter_tracker(
  result: Result(a, error.TrackerError),
  next: fn(a) -> Result(b, adapter.TrackerError),
) -> Result(b, adapter.TrackerError) {
  case result {
    Ok(value) -> next(value)
    Error(err) -> Error(test_adapter_tracker_error(err))
  }
}

fn test_adapter_tracker_error(err: error.TrackerError) -> adapter.TrackerError {
  case err {
    error.LinearApiStatus(status) ->
      case status == 429 || status >= 500 {
        True -> adapter.Transient(error.tracker_code(err))
        False -> adapter.Permanent(error.tracker_code(err))
      }
    _ -> adapter.Permanent(error.tracker_code(err))
  }
}

type TestClockMessage {
  GetNow(process.Subject(Int))
  SetNow(Int)
  StopClock
}

fn start_test_clock(initial_ms: Int) -> process.Subject(TestClockMessage) {
  let ready = process.new_subject()
  let _ =
    process.spawn(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      test_clock_loop(subject, initial_ms)
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
}

fn test_clock_loop(
  subject: process.Subject(TestClockMessage),
  now_ms: Int,
) -> Nil {
  case process.receive(subject, within: 5000) {
    Ok(GetNow(reply)) -> {
      process.send(reply, now_ms)
      test_clock_loop(subject, now_ms)
    }
    Ok(SetNow(next_ms)) -> test_clock_loop(subject, next_ms)
    Ok(StopClock) -> Nil
    Error(_) -> Nil
  }
}

fn clock_now(clock: process.Subject(TestClockMessage)) -> Int {
  let reply = process.new_subject()
  process.send(clock, GetNow(reply))
  let assert Ok(now_ms) = process.receive(reply, within: 1000)
  now_ms
}

fn set_clock(clock: process.Subject(TestClockMessage), now_ms: Int) -> Nil {
  process.send(clock, SetNow(now_ms))
}

type FetchRequest {
  FetchRequest(process.Subject(FetchDirective))
}

type FetchDirective {
  CrashFetch
  ReturnCandidates(List(tracker_issue.Issue))
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
      attempt_index,
      workspace_ref,
      orchestrator,
      profile,
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
        workflow_bundle_dir: "",
        attempt_index: attempt_index,
        workspace_name: workspace_ref.name,
        path: run_root <> "/" <> workspace_ref.name,
        source: workspace_source(workspace_ref.from, run_root),
        workspace_profile: profile.name,
      ))
    },
    prepare_recovered_step: fn(
      _issue,
      workflow_id,
      run_id,
      expected_run_root,
      _step_id,
      attempt_index,
      workspace_ref,
      _orchestrator,
      profile,
      _known,
    ) {
      Ok(workspace_run.PreparedStepWorkspace(
        workflow_id: workflow_id,
        run_id: run_id,
        run_root: expected_run_root,
        workflow_bundle_dir: ".scherzo/workflows",
        attempt_index: attempt_index,
        workspace_name: workspace_ref.name,
        path: expected_run_root <> "/" <> workspace_ref.name,
        source: workspace_source(workspace_ref.from, expected_run_root),
        workspace_profile: profile.name,
      ))
    },
    after_step: fn(_, step_id, _, _, _) {
      process.send(log_subject, "yaml_after:" <> step_id)
    },
    cleanup_run: fn(run_root, _, _) {
      process.send(log_subject, "yaml_cleanup:" <> run_root)
      Ok(Nil)
    },
    command_step: fn(
      context: workflow_run.StepContext,
      _command,
      _timeout,
      secrets,
      limits,
    ) {
      process.send(log_subject, "yaml_command:" <> context.step_id)
      step_artifact.from_command_result(
        context.step_id,
        0,
        "stdout:" <> context.step_id,
        "",
        False,
        secrets,
        limits,
      )
    },
    agent_step: fn(
      issue,
      context: workflow_run.StepContext,
      prompt_mode,
      _attempt_context,
      _effective,
      _tracker,
      emit_update,
      _command_ready,
      _record_pi_session,
    ) {
      let prompt = prompt_text(prompt_mode)
      process.send(log_subject, "yaml_agent:" <> prompt)
      emit_update(
        agent_types.RunnerPiUpdate(agent_types.PiUpdate(
          event: pi_event.TurnFinished,
          message: Some("hello"),
          raw_json: None,
          turn: Some(1),
          request_id: None,
          method: None,
          pi_session_id: None,
          tokens: session_tokens.zero_token_totals(),
          tool_name: None,
          tool_input: None,
          tool_output: None,
          tool_status: None,
        )),
      )
      Ok(agent_types.WorkerSuccess(
        final_issue: Some(issue),
        final_classification: agent_types.FinalTerminal,
        workspace_path: context.workspace_path,
        tokens: session_tokens.zero_token_totals(),
        turns: 1,
        result: result_artifact.from_final_response(Some(prompt), False, "test"),
      ))
    },
    checkpoint: workflow_checkpoint.noop_writer(),
  )
}

fn failing_command_workflow_run_dependencies(
  log_subject: process.Subject(String),
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
  workflow_run.Dependencies(
    ..base,
    command_step: fn(
      context: workflow_run.StepContext,
      _command,
      _timeout,
      secrets,
      limits,
    ) {
      process.send(log_subject, "yaml_command_failed:" <> context.step_id)
      step_artifact.from_command_result(
        context.step_id,
        1,
        "",
        "forced scheduled failure",
        False,
        secrets,
        limits,
      )
    },
  )
}

fn failing_agent_workflow_run_dependencies(
  log_subject: process.Subject(String),
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
  workflow_run.Dependencies(
    ..base,
    agent_step: fn(
      issue,
      context: workflow_run.StepContext,
      _prompt_mode,
      _attempt_context,
      _effective,
      _tracker,
      _emit_update,
      _command_ready,
      _record_pi_session,
    ) {
      process.send(log_subject, "yaml_agent_failed:" <> context.step_id)
      Error(agent_types.WorkerFailure(
        reason: error.PiFailed(error.PiProtocolError("forced_failure")),
        workspace_path: Some(context.workspace_path),
        tokens: session_tokens.zero_token_totals(),
        final_issue: Some(issue),
      ))
    },
  )
}

fn scheduled_reporter_success(
  report_subject: process.Subject(
    scheduled_failure_reporter.FailureReportRequest,
  ),
) -> scheduled_failure_reporter.Client {
  scheduled_failure_reporter.Client(report_failure: fn(request) {
    process.send(report_subject, request)
    Ok(scheduled_failure_reporter.FailureReportCreated("lin-scheduled"))
  })
}

type ScheduledReportDirective {
  ScheduledReportError
  ScheduledReportPermanentError
  ScheduledReportSuccess
}

type DirectedScheduledReportCall {
  DirectedScheduledReportCall(
    request: scheduled_failure_reporter.FailureReportRequest,
    reply: process.Subject(ScheduledReportDirective),
  )
}

fn scheduled_reporter_directed(
  report_subject: process.Subject(DirectedScheduledReportCall),
) -> scheduled_failure_reporter.Client {
  scheduled_failure_reporter.Client(report_failure: fn(request) {
    let reply = process.new_subject()
    process.send(report_subject, DirectedScheduledReportCall(request, reply))
    case process.receive(reply, within: 1000) {
      Ok(ScheduledReportSuccess) ->
        Ok(scheduled_failure_reporter.FailureReportUpdated("lin-scheduled"))
      Ok(ScheduledReportError) -> Error(error.LinearApiStatus(500))
      Ok(ScheduledReportPermanentError) -> Error(error.LinearApiRequest("boom"))
      Error(_) -> Error(error.LinearApiRequest("directive timeout"))
    }
  })
}

fn blocking_command_workflow_run_dependencies(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
  workflow_run.Dependencies(
    ..base,
    command_step: fn(
      context: workflow_run.StepContext,
      _command,
      _timeout,
      secrets,
      limits,
    ) {
      process.send(log_subject, "yaml_command:" <> context.step_id)
      test_async.block_until_released(barrier)
      step_artifact.from_command_result(
        context.step_id,
        0,
        "stdout:" <> context.step_id,
        "",
        False,
        secrets,
        limits,
      )
    },
  )
}

fn retry_failure_and_blocking_command_workflow_run_dependencies(
  command_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(command_subject)
  workflow_run.Dependencies(
    ..base,
    command_step: fn(
      context: workflow_run.StepContext,
      _command,
      _timeout,
      secrets,
      limits,
    ) {
      case string.starts_with(context.issue_id, "retry-") {
        True -> {
          process.send(
            command_subject,
            "issue_command_failed:" <> context.issue_id,
          )
          step_artifact.from_command_result(
            context.step_id,
            1,
            "",
            "forced retry failure",
            False,
            secrets,
            limits,
          )
        }
        False -> {
          case context.run_kind {
            "scheduled" ->
              process.send(
                command_subject,
                "scheduled_command:" <> context.scheduled_job_id,
              )
            _ ->
              process.send(
                command_subject,
                "issue_command:" <> context.issue_id,
              )
          }
          test_async.block_until_released(barrier)
          step_artifact.from_command_result(
            context.step_id,
            0,
            "stdout:" <> context.step_id,
            "",
            False,
            secrets,
            limits,
          )
        }
      }
    },
  )
}

fn blocking_command_ready_workflow_run_dependencies(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
  workflow_run.Dependencies(
    ..base,
    agent_step: fn(
      issue,
      context: workflow_run.StepContext,
      _prompt_mode,
      _attempt_context,
      _effective,
      _tracker,
      _emit_update,
      command_ready,
      _record_pi_session,
    ) {
      let command_subject = process.new_subject()
      command_ready(command_subject)
      process.send(log_subject, "agent_ready")
      test_async.block_until_released(barrier)
      Error(agent_types.WorkerFailure(
        reason: error.PiFailed(error.PiProtocolError(
          "stopped:" <> context.step_id,
        )),
        workspace_path: Some(context.workspace_path),
        tokens: session_tokens.zero_token_totals(),
        final_issue: Some(issue),
      ))
    },
  )
}

fn scheduled_parallel_failure_workflow_run_dependencies(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
  command_barrier: test_async.Barrier,
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
  workflow_run.Dependencies(
    ..base,
    command_step: fn(
      context: workflow_run.StepContext,
      _command,
      _timeout,
      secrets,
      limits,
    ) {
      test_async.block_until_released(command_barrier)
      process.send(log_subject, "yaml_command_failed:" <> context.step_id)
      step_artifact.from_command_result(
        context.step_id,
        1,
        "",
        "forced scheduled failure",
        False,
        secrets,
        limits,
      )
    },
    agent_step: fn(
      issue,
      context: workflow_run.StepContext,
      _prompt_mode,
      _attempt_context,
      _effective,
      _tracker,
      _emit_update,
      command_ready,
      _record_pi_session,
    ) {
      let command_subject = process.new_subject()
      command_ready(command_subject)
      process.send(log_subject, "agent_ready")
      test_async.block_until_released(barrier)
      process.send(log_subject, "agent_survived")
      Error(agent_types.WorkerFailure(
        reason: error.PiFailed(error.PiProtocolError(
          "survived:" <> context.step_id,
        )),
        workspace_path: Some(context.workspace_path),
        tokens: session_tokens.zero_token_totals(),
        final_issue: Some(issue),
      ))
    },
  )
}

fn surviving_agent_workflow_run_dependencies(
  log_subject: process.Subject(String),
  barrier: test_async.Barrier,
) -> workflow_run.Dependencies {
  let base = fake_workflow_run_dependencies(log_subject)
  workflow_run.Dependencies(
    ..base,
    agent_step: fn(
      issue,
      context: workflow_run.StepContext,
      _prompt_mode,
      _attempt_context,
      _effective,
      _tracker,
      _emit_update,
      _command_ready,
      _record_pi_session,
    ) {
      process.send(log_subject, "agent_started")
      test_async.block_until_released(barrier)
      process.send(log_subject, "agent_survived")
      Error(agent_types.WorkerFailure(
        reason: error.PiFailed(error.PiProtocolError("survived_abort")),
        workspace_path: Some(context.workspace_path),
        tokens: session_tokens.zero_token_totals(),
        final_issue: Some(issue),
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
      context: workflow_run.StepContext,
      prompt_mode,
      _attempt_context,
      _effective,
      _tracker,
      _emit_update,
      command_ready,
      _record_pi_session,
    ) {
      let prompt = prompt_text(prompt_mode)
      let command_subject = process.new_subject()
      command_ready(command_subject)
      process.send(log_subject, "agent_ready")
      case process.receive(command_subject, within: 5000) {
        Ok(worker_command.QueuePrompt(message, reply)) -> {
          process.send(log_subject, "prompt:" <> message)
          process.send(reply, worker_command.Queued(Some("queued")))
          Ok(agent_types.WorkerSuccess(
            final_issue: Some(issue),
            final_classification: agent_types.FinalTerminal,
            workspace_path: context.workspace_path,
            tokens: session_tokens.zero_token_totals(),
            turns: 1,
            result: result_artifact.from_final_response(
              Some(prompt <> ":" <> message),
              False,
              "test",
            ),
          ))
        }
        Ok(other) -> {
          let _ = other
          Error(agent_types.WorkerFailure(
            reason: error.PiFailed(error.PiProtocolError("unexpected_command")),
            workspace_path: Some(context.workspace_path),
            tokens: session_tokens.zero_token_totals(),
            final_issue: Some(issue),
          ))
        }
        Error(_) ->
          Error(agent_types.WorkerFailure(
            reason: error.PiFailed(error.PiProtocolError("command_timeout")),
            workspace_path: Some(context.workspace_path),
            tokens: session_tokens.zero_token_totals(),
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
      _context,
      _prompt_mode,
      _attempt_context,
      _effective,
      _tracker,
      _emit_update,
      command_ready,
      _record_pi_session,
    ) {
      let command_subject = process.new_subject()
      command_ready(command_subject)
      process.send(log_subject, "agent_ready")
      panic as "yaml agent crashed"
    },
  )
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

fn wait_for_session_status(
  subject: process.Subject(hub.Message),
  session_id: String,
  status: event.SessionStatus,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case hub.get_session(subject, session_id, 100) {
        Ok(Some(summary)) ->
          case summary.status == status {
            True -> True
            False -> {
              process.sleep(50)
              wait_for_session_status(subject, session_id, status, attempts - 1)
            }
          }
        _ -> {
          process.sleep(50)
          wait_for_session_status(subject, session_id, status, attempts - 1)
        }
      }
  }
}

fn find_event(
  events: List(event.SessionEvent),
  name: String,
) -> Option(event.SessionEvent) {
  case events {
    [] -> None
    [stored_event, ..rest] ->
      case
        event.name_to_string(event.payload_name(stored_event.payload)) == name
      {
        True -> Some(stored_event)
        False -> find_event(rest, name)
      }
  }
}

fn wait_for_event(
  subject: process.Subject(String),
  event: String,
  quiet_attempts: Int,
) -> Bool {
  case wait_for_event_result(subject, event, quiet_attempts) {
    Ok(_) -> True
    Error(_) -> False
  }
}

fn wait_for_metrics(
  daemon_subject: process.Subject(daemon.Message),
  attempts: Int,
  predicate: fn(query_types.OperationalMetricsDto) -> Bool,
) -> Result(query_types.OperationalMetricsDto, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case daemon.execute_query(daemon_subject, query_types.Metrics, 1000) {
        Ok(query_types.MetricsResponse(metrics)) ->
          case predicate(metrics) {
            True -> Ok(metrics)
            False -> {
              process.sleep(50)
              wait_for_metrics(daemon_subject, attempts - 1, predicate)
            }
          }
        _ -> {
          process.sleep(50)
          wait_for_metrics(daemon_subject, attempts - 1, predicate)
        }
      }
  }
}

fn wait_for_events(
  subject: process.Subject(String),
  events: List(String),
  quiet_attempts: Int,
) -> Bool {
  case events, quiet_attempts <= 0 {
    [], _ -> True
    _, True -> False
    _, False ->
      case process.receive(subject, within: 500) {
        Ok(received) ->
          wait_for_events(
            subject,
            list.filter(events, fn(event) { event != received }),
            quiet_attempts,
          )
        Error(_) -> wait_for_events(subject, events, quiet_attempts - 1)
      }
  }
}

fn wait_for_event_result(
  subject: process.Subject(String),
  event: String,
  quiet_attempts: Int,
) -> Result(List(String), List(String)) {
  wait_for_event_result_loop(subject, event, quiet_attempts, [])
}

fn wait_for_event_prefix(
  subject: process.Subject(String),
  prefix: String,
  quiet_attempts: Int,
) -> Result(String, List(String)) {
  wait_for_event_prefix_loop(subject, prefix, quiet_attempts, [])
}

fn wait_for_log_fields(
  subject: process.Subject(#(String, List(#(String, String)))),
  event: String,
  quiet_attempts: Int,
) -> Result(List(#(String, String)), Nil) {
  case quiet_attempts <= 0 {
    True -> Error(Nil)
    False ->
      case process.receive(subject, within: 500) {
        Ok(#(received, fields)) ->
          case received == event {
            True -> Ok(fields)
            False -> wait_for_log_fields(subject, event, quiet_attempts - 1)
          }
        Error(_) -> wait_for_log_fields(subject, event, quiet_attempts - 1)
      }
  }
}

fn wait_for_event_result_loop(
  subject: process.Subject(String),
  event: String,
  quiet_attempts: Int,
  seen: List(String),
) -> Result(List(String), List(String)) {
  case quiet_attempts <= 0 {
    True -> Error(list.reverse(seen))
    False ->
      case process.receive(subject, within: 500) {
        Ok(received) -> {
          let seen = [received, ..seen]
          case received == event {
            True -> Ok(list.reverse(seen))
            False ->
              wait_for_event_result_loop(subject, event, quiet_attempts, seen)
          }
        }
        Error(_) ->
          wait_for_event_result_loop(subject, event, quiet_attempts - 1, seen)
      }
  }
}

fn wait_for_event_prefix_loop(
  subject: process.Subject(String),
  prefix: String,
  quiet_attempts: Int,
  seen: List(String),
) -> Result(String, List(String)) {
  case quiet_attempts <= 0 {
    True -> Error(list.reverse(seen))
    False ->
      case process.receive(subject, within: 500) {
        Ok(received) -> {
          let seen = [received, ..seen]
          case string.starts_with(received, prefix) {
            True -> Ok(received)
            False ->
              wait_for_event_prefix_loop(subject, prefix, quiet_attempts, seen)
          }
        }
        Error(_) ->
          wait_for_event_prefix_loop(subject, prefix, quiet_attempts - 1, seen)
      }
  }
}

fn wait_for_event_without_event(
  subject: process.Subject(String),
  wanted: String,
  forbidden: String,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case process.receive(subject, within: 500) {
        Ok(received) ->
          case received == forbidden {
            True -> False
            False ->
              case received == wanted {
                True -> True
                False ->
                  wait_for_event_without_event(
                    subject,
                    wanted,
                    forbidden,
                    attempts - 1,
                  )
              }
          }
        Error(_) -> False
      }
  }
}

fn wait_for_monitor_down(monitor: process.Monitor, timeout_ms: Int) -> Bool {
  let selector =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { True })
  case process.selector_receive(selector, within: timeout_ms) {
    Ok(True) -> True
    Ok(False) -> False
    Error(_) -> False
  }
}

fn load_test_projection(root: String) -> projection.Projection {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(projected) = ledger.load_projection(ledger_path)
  projected
}

fn load_test_records(root: String) -> List(record.LedgerRecord) {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(read) = ledger.read_records(ledger_path)
  read.records
}

fn ledger_kinds(root: String) -> Result(List(String), String) {
  case ledger.path_for_workspace_root(root) {
    Error(_) -> Error("ledger_path_failed")
    Ok(ledger_path) ->
      case ledger.read_records(ledger_path) {
        Error(_) -> Error("ledger_read_failed")
        Ok(read) ->
          Ok(
            list.map(read.records, fn(ledger_record) {
              record.kind(ledger_record.body)
            }),
          )
      }
  }
}

fn write_corrupt_current_ledger(root: String) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(ledger_path.ledger_dir)
  let assert Ok(Nil) =
    simplifile.write(ledger_path.current_path, "not-json\nnot-json\n")
  Nil
}

fn create_current_ledger_directory(root: String) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let assert Ok(Nil) = simplifile.create_directory_all(ledger_path.current_path)
  Nil
}

fn workflow_finished_outcomes(root: String) -> List(String) {
  load_test_records(root)
  |> list.filter_map(fn(ledger_record) {
    case ledger_record.body {
      record.WorkflowRunFinished(outcome: outcome, ..)
      | record.WorkflowRunFinishedWithTask(outcome: outcome, ..) -> Ok(outcome)
      _ -> Error(Nil)
    }
  })
}

fn workflow_finished_task_refs(root: String) -> List(record.TaskRefFields) {
  load_test_records(root)
  |> list.filter_map(fn(ledger_record) {
    case ledger_record.body {
      record.WorkflowRunFinishedWithTask(task_ref: task_ref, ..) -> Ok(task_ref)
      _ -> Error(Nil)
    }
  })
}

fn legacy_terminal_record_kinds(root: String) -> List(String) {
  load_test_records(root)
  |> list.filter_map(fn(ledger_record) {
    case ledger_record.body {
      record.RunFinished(..) -> Ok("run_finished")
      record.WorkflowRunFinished(..) -> Ok("workflow_run_finished")
      _ -> Error(Nil)
    }
  })
}

fn wait_for_workflow_finished_outcomes(
  root: String,
  expected: List(String),
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False ->
      case workflow_finished_outcomes(root) == expected {
        True -> True
        False -> {
          process.sleep(50)
          wait_for_workflow_finished_outcomes(root, expected, attempts - 1)
        }
      }
  }
}

fn append_test_ledger_bodies(
  root: String,
  bodies: List(record.RecordBody),
) -> Nil {
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(root)
  let records = test_records_for_bodies(bodies, 100, 1)
  let assert Ok(Nil) = ledger.append_many(ledger_path, records, True)
  Nil
}

fn test_records_for_bodies(
  bodies: List(record.RecordBody),
  at_ms: Int,
  sequence: Int,
) -> List(record.LedgerRecord) {
  case bodies {
    [] -> []
    [body, ..rest] -> [
      record.new(at_ms, sequence, body),
      ..test_records_for_bodies(rest, at_ms + 100, sequence + 1)
    ]
  }
}

fn append_succeeded_scheduled_run(
  root: String,
  due_at_ms: Int,
  run_id: String,
) -> Nil {
  append_test_ledger_bodies(root, [
    record.ScheduledJobDue(
      "scheduled-job",
      "implementation",
      due_at_ms,
      run_id,
      "automatic",
    ),
    record.ScheduledRunPending(
      "scheduled-job",
      "implementation",
      due_at_ms,
      run_id,
      "automatic",
      due_at_ms,
    ),
    record.ScheduledRunStarted(
      "scheduled-job",
      "implementation",
      due_at_ms,
      due_at_ms + 100,
      run_id,
      1,
      run_id <> "-a1",
      root <> "/implementation/scheduled/scheduled-job/" <> run_id,
    ),
    record.ScheduledRunSucceeded(
      "scheduled-job",
      "implementation",
      due_at_ms,
      run_id,
      1,
      due_at_ms + 200,
      0,
      0,
    ),
  ])
}

fn append_started_scheduled_run(root: String, run_id: String) -> Nil {
  append_test_ledger_bodies(root, [
    record.ScheduledJobDue(
      "scheduled-job",
      "implementation",
      1000,
      run_id,
      "automatic",
    ),
    record.ScheduledRunPending(
      "scheduled-job",
      "implementation",
      1000,
      run_id,
      "automatic",
      1000,
    ),
    record.ScheduledRunStarted(
      "scheduled-job",
      "implementation",
      1000,
      1100,
      run_id,
      1,
      run_id <> "-a1",
      root <> "/implementation/scheduled/scheduled-job/" <> run_id,
    ),
  ])
}

fn append_retry_waiting_scheduled_run(root: String, run_id: String) -> Nil {
  append_test_ledger_bodies(root, [
    record.ScheduledJobDue(
      "scheduled-job",
      "implementation",
      1000,
      run_id,
      "automatic",
    ),
    record.ScheduledRunPending(
      "scheduled-job",
      "implementation",
      1000,
      run_id,
      "automatic",
      1000,
    ),
    record.ScheduledRunStarted(
      "scheduled-job",
      "implementation",
      1000,
      1100,
      run_id,
      1,
      run_id <> "-a1",
      root <> "/implementation/scheduled/scheduled-job/" <> run_id,
    ),
    record.ScheduledRunFailed(
      "scheduled-job",
      "implementation",
      1000,
      run_id,
      1,
      1200,
      "workflow_command_failed",
      False,
      Some(root <> "/implementation/scheduled/scheduled-job/" <> run_id),
    ),
    record.ScheduledRunRetryScheduled(
      "scheduled-job",
      "implementation",
      1000,
      run_id,
      2,
      10_000,
      1,
      "workflow_command_failed",
    ),
  ])
}

fn has_scheduled_due(
  records: List(record.LedgerRecord),
  run_id: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledJobDue(_, _, _, body_run_id, _) -> body_run_id == run_id
      _ -> False
    }
  })
}

fn has_scheduled_pending(
  records: List(record.LedgerRecord),
  run_id: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledRunPending(_, _, _, body_run_id, _, _) ->
        body_run_id == run_id
      _ -> False
    }
  })
}

fn has_scheduled_started(
  records: List(record.LedgerRecord),
  run_id: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledRunStarted(_, _, _, _, body_run_id, _, _, _) ->
        body_run_id == run_id
      _ -> False
    }
  })
}

fn scheduled_started_count(records: List(record.LedgerRecord)) -> Int {
  records
  |> list.filter(fn(entry) {
    case entry.body {
      record.ScheduledRunStarted(_, _, _, _, _, _, _, _) -> True
      _ -> False
    }
  })
  |> list.length
}

fn has_scheduled_succeeded(
  records: List(record.LedgerRecord),
  run_id: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledRunSucceeded(_, _, _, body_run_id, _, _, _, _) ->
        body_run_id == run_id
      _ -> False
    }
  })
}

fn has_scheduled_failed(
  records: List(record.LedgerRecord),
  run_id: String,
  reason: String,
  retry_exhausted: Bool,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledRunFailed(
        _,
        _,
        _,
        body_run_id,
        _,
        _,
        body_reason,
        body_retry_exhausted,
        _,
      ) ->
        body_run_id == run_id
        && body_reason == reason
        && body_retry_exhausted == retry_exhausted
      _ -> False
    }
  })
}

fn has_scheduled_failed_run(
  records: List(record.LedgerRecord),
  run_id: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledRunFailed(_, _, _, body_run_id, _, _, _, _, _) ->
        body_run_id == run_id
      _ -> False
    }
  })
}

fn has_scheduled_retry_scheduled(
  records: List(record.LedgerRecord),
  run_id: String,
  next_attempt: Int,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledRunRetryScheduled(
        _,
        _,
        _,
        body_run_id,
        body_next_attempt,
        _,
        _,
        _,
      ) -> body_run_id == run_id && body_next_attempt == next_attempt
      _ -> False
    }
  })
}

fn has_scheduled_failure_reported(
  records: List(record.LedgerRecord),
  run_id: String,
  issue_id: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledFailureReported(
        _,
        _,
        _,
        body_run_id,
        _,
        _,
        body_issue_id,
        _,
      ) -> body_run_id == run_id && body_issue_id == issue_id
      _ -> False
    }
  })
}

fn has_scheduled_failure_report_failed(
  records: List(record.LedgerRecord),
  run_id: String,
  generation: Int,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledFailureReportFailed(
        _,
        _,
        _,
        body_run_id,
        _,
        _,
        _,
        _,
        _,
        body_generation,
      ) -> body_run_id == run_id && body_generation == generation
      _ -> False
    }
  })
}

fn has_terminal_scheduled_failure_report_failed(
  records: List(record.LedgerRecord),
  run_id: String,
  generation: Int,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledFailureReportFailed(
        _,
        _,
        _,
        body_run_id,
        _,
        _,
        _,
        _,
        next_retry_at_ms,
        body_generation,
      ) ->
        body_run_id == run_id
        && body_generation == generation
        && next_retry_at_ms == 0
      _ -> False
    }
  })
}

fn has_scheduled_failure_outbox_attempted(
  records: List(record.LedgerRecord),
  attempt_count: Int,
) -> Bool {
  let key = scheduled_failure_reporter.dedupe_key("scheduled-job")
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.OutboxAttemptedWithTask(
        outbox_id: outbox_id,
        outbox_kind: outbox_kind,
        dedupe_key: dedupe_key,
        attempt_count: body_attempt_count,
        ..,
      ) ->
        outbox_id == key
        && dedupe_key == key
        && outbox_kind == outbox.scheduled_failure_publication_kind
        && body_attempt_count == attempt_count
      _ -> False
    }
  })
}

fn has_scheduled_failure_outbox_completed(
  records: List(record.LedgerRecord),
) -> Bool {
  let key = scheduled_failure_reporter.dedupe_key("scheduled-job")
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.OutboxCompletedWithTask(
        outbox_id: outbox_id,
        outbox_kind: outbox_kind,
        ..,
      ) ->
        outbox_id == key
        && outbox_kind == outbox.scheduled_failure_publication_kind
      _ -> False
    }
  })
}

fn has_scheduled_failure_outbox_retry(
  records: List(record.LedgerRecord),
  attempt_count: Int,
) -> Bool {
  let key = scheduled_failure_reporter.dedupe_key("scheduled-job")
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.OutboxRetryScheduledWithTask(
        outbox_id: outbox_id,
        outbox_kind: outbox_kind,
        dedupe_key: dedupe_key,
        attempt_count: body_attempt_count,
        next_attempt_at_ms: next_attempt_at_ms,
        ..,
      ) ->
        outbox_id == key
        && dedupe_key == key
        && outbox_kind == outbox.scheduled_failure_publication_kind
        && body_attempt_count == attempt_count
        && next_attempt_at_ms > 0
      _ -> False
    }
  })
}

fn has_scheduled_failure_outbox_permanent(
  records: List(record.LedgerRecord),
  attempt_count: Int,
) -> Bool {
  let key = scheduled_failure_reporter.dedupe_key("scheduled-job")
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.OutboxPermanentlyFailedWithTask(
        outbox_id: outbox_id,
        outbox_kind: outbox_kind,
        attempt_count: body_attempt_count,
        ..,
      ) ->
        outbox_id == key
        && outbox_kind == outbox.scheduled_failure_publication_kind
        && body_attempt_count == attempt_count
      _ -> False
    }
  })
}

fn scheduled_failure_report_failed_count(
  records: List(record.LedgerRecord),
  run_id: String,
) -> Int {
  records
  |> list.filter(fn(entry) {
    case entry.body {
      record.ScheduledFailureReportFailed(
        _,
        _,
        _,
        body_run_id,
        _,
        _,
        _,
        _,
        _,
        _,
      ) -> body_run_id == run_id
      _ -> False
    }
  })
  |> list.length
}

fn has_scheduled_skip(
  records: List(record.LedgerRecord),
  reason: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.ScheduledJobSkipped(_, _, _, _, body_reason, _) ->
        body_reason == reason
      _ -> False
    }
  })
}

fn has_step_success(
  records: List(record.LedgerRecord),
  step_id: String,
) -> Bool {
  records
  |> list.any(fn(entry) {
    case entry.body {
      record.StepAttemptFinished(
        _,
        _,
        body_step_id,
        _,
        outcome,
        _,
        _,
        _,
        _,
        _,
        _,
      ) -> body_step_id == step_id && outcome == "completed"
      _ -> False
    }
  })
}

fn wait_for_records(
  root: String,
  predicate: fn(List(record.LedgerRecord)) -> Bool,
  attempts: Int,
) -> Bool {
  case attempts <= 0 {
    True -> False
    False -> {
      let records = load_test_records(root)
      case predicate(records) {
        True -> True
        False -> {
          process.sleep(50)
          wait_for_records(root, predicate, attempts - 1)
        }
      }
    }
  }
}

fn start_stuck_event_hub() -> process.Subject(hub.Message) {
  let ready = process.new_subject()
  let barrier = test_async.new_barrier()
  let _pid =
    process.spawn_unlinked(fn() {
      let subject = process.new_subject()
      process.send(ready, subject)
      test_async.block_until_released(barrier)
    })
  let assert Ok(subject) = process.receive(ready, within: 1000)
  subject
}

pub fn daemon_shutdown_stops_event_hub_test() {
  let workflow_path = write_workflow("test/tmp/daemon-event-hub-shutdown", 1)
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let assert Ok(event_hub_pid) = process.subject_owner(event_hub)
  let event_hub_monitor = process.monitor(event_hub_pid)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  let stopped = wait_for_monitor_down(event_hub_monitor, 1000)
  case stopped {
    True -> Nil
    False -> hub.stop(event_hub)
  }
  process.demonitor_process(event_hub_monitor)
  assert stopped
}

pub fn daemon_shutdown_logs_event_hub_timeout_test() {
  let workflow_path =
    write_workflow("test/tmp/daemon-event-hub-shutdown-timeout", 1)
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let event_hub = start_stuck_event_hub()
  let assert Ok(event_hub_pid) = process.subject_owner(event_hub)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  assert daemon.shutdown(started.data, 3000) == Ok(Nil)
  assert wait_for_event(log_subject, "event_hub_shutdown_timeout", 10)
  process.kill(event_hub_pid)
}

pub fn daemon_shutdown_uses_cached_state_after_post_start_ledger_corruption_test() {
  let root = "test/tmp/daemon-shutdown-projection-unavailable"
  let workflow_path = write_workflow(root, 1)
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let deps = base_dependencies(client, log_subject)
  let assert Ok(bundle) = runtime_bundle.load(Some(workflow_path))
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)
  let assert Ok(ledger_path) =
    ledger.path_for_workspace_root(bundle.effective.workspace.root)
  let assert Ok(Nil) = simplifile.create_directory_all(ledger_path.ledger_dir)
  let assert Ok(Nil) =
    simplifile.write(ledger_path.current_path, "not-json\nnot-json\n")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  let logs = test_async.drain_subject(log_subject)
  assert !list.contains(logs, "workflow_shutdown_projection_unavailable")
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
      make_tracker_adapter: fn(_) {
        adapter_with_invalid_comment_subject(client, triage_subject)
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  assert process.receive(triage_subject, within: 1000)
    == Ok("triage:issue-id:missing_workflow_label")
  let identity = orchestrator_state.issue_identity(candidate)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(snapshot.running) == 0
  assert dict.has_key(snapshot.invalid_workflow_reports, identity)

  process.send(started.data, daemon.PollTick(2))
  test_async.assert_no_extra_message_within(triage_subject, 200)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_ignores_unlabeled_non_dispatch_state_candidate_test() {
  let workflow_path =
    write_enforcing_split_state_workflow(
      "test/tmp/daemon-invalid-in-progress-ignored",
      1,
    )
  let candidate = issue("issue-id", "ABC-1", "In Progress")
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        process.send(refresh_subject, "refresh")
        Ok([candidate])
      },
    )
  let log_subject = process.new_subject()
  let triage_subject = process.new_subject()
  let deps = base_dependencies(client, log_subject)
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  test_async.assert_no_extra_message_within(triage_subject, 200)
  test_async.assert_no_extra_message_within(refresh_subject, 200)
  let identity = orchestrator_state.issue_identity(candidate)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(snapshot.running) == 0
  assert !dict.has_key(snapshot.invalid_workflow_reports, identity)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_ignores_workflow_labeled_non_dispatch_state_candidate_test() {
  let workflow_path =
    write_enforcing_split_state_workflow(
      "test/tmp/daemon-valid-in-progress-ignored",
      1,
    )
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "In Progress"), labels: [
      "workflow:implementation",
    ])
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        process.send(refresh_subject, "refresh")
        Ok([candidate])
      },
    )
  let log_subject = process.new_subject()
  let triage_subject = process.new_subject()
  let deps = base_dependencies(client, log_subject)
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  test_async.assert_no_extra_message_within(triage_subject, 200)
  test_async.assert_no_extra_message_within(refresh_subject, 200)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(snapshot.running) == 0
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_reports_invalid_workflow_candidate_when_slots_are_full_test() {
  let workflow_path =
    write_enforcing_workflow("test/tmp/daemon-invalid-workflow-full-slots", 1)
  let valid_candidate =
    tracker_issue.Issue(..issue("valid-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
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
      make_tracker_adapter: fn(_) {
        adapter_with_invalid_comment_subject(client, triage_subject)
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  assert process.receive(triage_subject, within: 1000)
    == Ok("triage:invalid-id:missing_workflow_label")
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_dispatches_valid_workflow_candidate_test() {
  let workflow_path =
    write_enforcing_workflow("test/tmp/daemon-valid-workflow", 1)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let triage_subject = process.new_subject()
  let deps = base_dependencies(client, log_subject)
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "dispatch_started", 10)
  test_async.assert_no_extra_message_within(triage_subject, 100)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_claim_handoff_appends_parent_records_before_worker_spawn_test() {
  let dir = "test/tmp/daemon-claim-handoff-ledger"
  let workflow_path = write_workflow(dir, 1)
  let assert Ok(bundle) = runtime_bundle.load(Some(workflow_path))
  let root = bundle.effective.workspace.root
  let candidate = issue("issue-id", "ABC-1", "Todo")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let claim_subject = process.new_subject()
  let ledger_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_tracker_adapter: fn(_) {
        adapter_with_handoff_claim_subject(client, claim_subject)
      },
      logger: fn(_, event, _, _) {
        process.send(log_subject, event)
        case event == "dispatch_started" {
          True -> process.send(ledger_subject, ledger_kinds(root))
          False -> Nil
        }
        Ok(Nil)
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  assert process.receive(claim_subject, within: 1000)
    == Ok(#("issue-id", "ABC-1-42-1"))
  let assert Ok(Ok(kinds_before_spawn)) =
    process.receive(ledger_subject, within: 1000)
  assert kinds_before_spawn
    == [
      "outbox_pending_v2",
      "outbox_attempted",
      "workflow_run_started",
      "known_workspace",
      "issue_counter_updated",
      "outbox_completed",
    ]

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_final_validation_blocks_new_dependency_test() {
  let workflow_path =
    write_enforcing_workflow("test/tmp/daemon-validation-blocked", 1)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let refreshed =
    tracker_issue.Issue(
      ..candidate,
      blocked_by: [
        tracker_issue.BlockerRef(
          id: Some("blocker-id"),
          identifier: Some("ABC-0"),
          state: Some(issue_state.from_string_unchecked("Todo")),
        ),
      ],
      blocked_by_complete: False,
    )
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        let reply = process.new_subject()
        process.send(refresh_subject, reply)
        case process.receive(reply, within: 1000) {
          Ok(issue) -> Ok([issue])
          Error(_) -> Error(error.LinearApiRequest("refresh timeout"))
        }
      },
    )
  let log_subject = process.new_subject()
  let claim_subject = process.new_subject()
  let deps = base_dependencies(client, log_subject)
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(reply) = process.receive(refresh_subject, within: 1000)
  process.send(reply, refreshed)

  assert wait_for_event(
    log_subject,
    "linear_dependency_claim_validation_blocked",
    20,
  )
  test_async.assert_no_extra_message_within(claim_subject, 100)
  let identity = orchestrator_state.issue_identity(candidate)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.size(snapshot.running) == 0
  assert !dict.has_key(snapshot.claimed, identity)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_final_validation_allows_terminal_blocker_test() {
  let workflow_path =
    write_enforcing_workflow("test/tmp/daemon-validation-terminal", 1)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let refreshed =
    tracker_issue.Issue(..candidate, title: "Refreshed title", blocked_by: [
      tracker_issue.BlockerRef(
        id: Some("blocker-id"),
        identifier: Some("ABC-0"),
        state: Some(issue_state.from_string_unchecked("Done")),
      ),
    ])
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        let reply = process.new_subject()
        process.send(refresh_subject, reply)
        case process.receive(reply, within: 1000) {
          Ok(issue) -> Ok([issue])
          Error(_) -> Error(error.LinearApiRequest("refresh timeout"))
        }
      },
    )
  let log_subject = process.new_subject()
  let deps = base_dependencies(client, log_subject)
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(reply) = process.receive(refresh_subject, within: 1000)
  process.send(reply, refreshed)

  assert wait_for_event(log_subject, "dispatch_started", 20)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_worker_failure_parks_without_retry_refresh_test() {
  let workflow_path = write_workflow("test/tmp/daemon-retry-dependency", 1)
  let retried = issue("retry-id", "ABC-2", "Todo")
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([retried]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        let reply = process.new_subject()
        process.send(refresh_subject, reply)
        case process.receive(reply, within: 1000) {
          Ok(issue) -> Ok([issue])
          Error(_) -> Error(error.LinearApiRequest("refresh timeout"))
        }
      },
    )
  let log_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: workflow_run.Dependencies(
        ..fake_workflow_run_dependencies(log_subject),
        agent_step: fn(
          _issue: tracker_issue.Issue,
          context: workflow_run.StepContext,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
        ) {
          Error(agent_types.WorkerFailure(
            reason: error.PiFailed(error.PiProtocolError("boom")),
            workspace_path: Some(context.workspace_path),
            tokens: session_tokens.zero_token_totals(),
            final_issue: None,
          ))
        },
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(initial_refresh) =
    process.receive(refresh_subject, within: 1000)
  process.send(initial_refresh, retried)
  assert wait_for_event(log_subject, "issue_parked", 20)

  let identity = orchestrator_state.issue_identity(retried)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert !dict.has_key(snapshot.retry_attempts, identity)
  assert !dict.has_key(snapshot.claimed, identity)
  assert dict.has_key(snapshot.parked, identity)
  test_async.assert_no_extra_message_within(refresh_subject, 100)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_yaml_agent_steps_get_concrete_sessions_test() {
  let dir = "test/tmp/daemon-yaml-agent-session"
  let workflow_path = write_yaml_agent_workflow(dir)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "worker_exited", 20)
  let assert Ok(sessions) = hub.list_sessions(event_hub, 1000)
  let session_ids = list.map(sessions, fn(summary) { summary.session_id })
  assert list.contains(session_ids, "ABC-1-42-1")
  assert list.contains(
    session_ids,
    "workflow-step-ABC-1-42-1-implement-a1-f9bb818d8483",
  )
  let matching_step_sessions =
    list.filter(sessions, fn(summary) {
      summary.session_id == "workflow-step-ABC-1-42-1-implement-a1-f9bb818d8483"
    })
  let assert [_step_session] = matching_step_sessions
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_yaml_operator_prompt_routes_to_agent_step_session_test() {
  let dir = "test/tmp/daemon-yaml-agent-command"
  let workflow_path = write_yaml_agent_workflow(dir)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
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
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "agent_ready", 20)

  let result =
    prompt_until_queued(
      started.data,
      "workflow-step-ABC-1-42-1-implement-a1-f9bb818d8483",
      20,
    )
  assert result.status == command.Queued
  assert wait_for_event(log_subject, "prompt:hello from operator", 20)
  assert wait_for_event(log_subject, "worker_exited", 20)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_stale_worker_finished_keeps_active_worker_commandable_test() {
  let dir = "test/tmp/daemon-stale-worker-finished"
  let workflow_path = write_yaml_agent_workflow(dir)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let log_fields_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: command_ready_workflow_run_dependencies(
        log_subject,
      ),
      logger: fn(_, event, fields, _) {
        process.send(log_subject, event)
        process.send(log_fields_subject, #(event, fields))
        Ok(Nil)
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "agent_ready", 20)
  process.send(
    started.data,
    daemon.WorkerFinished(
      "issue-id",
      "stale-run",
      Error(agent_types.WorkerFailure(
        reason: error.PiFailed(error.PiProtocolError("stale finish")),
        workspace_path: Some("stale-workspace"),
        tokens: session_tokens.zero_token_totals(),
        final_issue: Some(candidate),
      )),
    ),
  )

  let assert Ok(fields) =
    wait_for_log_fields(log_fields_subject, "worker_finished_stale", 20)
  let field_map = dict.from_list(fields)
  assert dict.get(field_map, "issue_id") == Ok("issue-id")
  assert dict.get(field_map, "run_id") == Ok("stale-run")
  let result = prompt_until_queued(started.data, "ABC-1-42-1", 20)
  assert result.status == command.Queued
  assert wait_for_event(log_subject, "worker_exited", 20)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_yaml_parent_prompt_rejects_multiple_active_step_routes_test() {
  let dir = "test/tmp/daemon-yaml-parent-command-multiple"
  let workflow_path = write_parallel_yaml_agent_workflow(dir)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: blocking_command_ready_workflow_run_dependencies(
        log_subject,
        worker_barrier,
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "agent_ready", 20)
  assert wait_for_event(log_subject, "agent_ready", 20)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.PromptSession("ABC-1-42-1", "hello from operator"),
      1000,
    )
  assert result.status == command.NotAllowed("multiple_step_command_subjects")
  test_async.release_barrier(worker_barrier)
  test_async.release_barrier(worker_barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_yaml_parent_abort_kills_active_step_worker_test() {
  let dir = "test/tmp/daemon-yaml-parent-abort"
  let workflow_path = write_yaml_agent_workflow(dir)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: surviving_agent_workflow_run_dependencies(
        log_subject,
        worker_barrier,
      ),
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "agent_started", 20)
  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.AbortSession("ABC-1-42-1"),
      1000,
    )
  assert result.status == command.Applied
  test_async.release_barrier_if_waiting_within(worker_barrier, 100)
  assert wait_for_session_status(
    event_hub,
    "workflow-step-ABC-1-42-1-implement-a1-f9bb818d8483",
    event.Exited(reason.OperatorAbort),
    20,
  )
  let post_abort_logs = test_async.drain_subject(log_subject)
  assert !list.contains(post_abort_logs, "agent_survived")
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  hub.stop(event_hub)
}

pub fn daemon_yaml_agent_step_crash_cleans_command_route_test() {
  use <- expected_crash.suppressing([
    "test/orchestrator_daemon_test.gleam",
    "crashing_command_ready_workflow_run_dependencies",
    "yaml agent crashed",
  ])
  let dir = "test/tmp/daemon-yaml-agent-command-crash"
  let workflow_path = write_yaml_agent_workflow(dir)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
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
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(log_subject, "agent_ready", 20)
  assert wait_for_event(log_subject, "worker_exited", 20)

  let assert Ok(result) =
    daemon.apply_operator_command(
      started.data,
      command.PromptSession(
        "workflow-step-ABC-1-42-1-implement-a1-f9bb818d8483",
        "after crash",
      ),
      1000,
    )
  assert result.status == command.NotFound
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_scheduled_startup_uses_replayed_statuses_after_event_hub_start_test() {
  let dir = "test/tmp/daemon-scheduled-startup-projection-unavailable"
  let workflow_path = write_scheduled_command_workflow(dir, 1)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client = empty_tracker_client()
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: fake_workflow_run_dependencies(command_subject),
      now_ms: fn() { clock_now(clock) },
      start_event_hub: fn() {
        write_corrupt_current_ledger(root)
        hub.start(10, fn() { 42 })
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)
  let startup_logs = test_async.drain_subject(log_subject)
  assert !list.contains(
    startup_logs,
    "scheduled_next_due_projection_unavailable",
  )
  assert !list.contains(
    startup_logs,
    "scheduled_runtime_recovery_projection_unavailable",
  )

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "scheduled_due_append_failed", 20)
  assert wait_for_event(log_subject, "scheduled_pending_append_failed", 20)
  assert wait_for_event(log_subject, "scheduled_started_append_failed", 20)
  assert wait_for_event(command_subject, "yaml_command:scheduled_command", 20)
  assert wait_for_event(log_subject, "scheduled_worker_exited", 20)
  assert wait_for_event(log_subject, "scheduled_success_append_failed", 20)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_append_failure_logs_and_continues_test() {
  let dir = "test/tmp/daemon-scheduled-append-failure"
  let workflow_path = write_scheduled_command_workflow(dir, 1)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client = empty_tracker_client()
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: fake_workflow_run_dependencies(command_subject),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)
  create_current_ledger_directory(root)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "scheduled_due_append_failed", 20)
  assert wait_for_event(log_subject, "scheduled_pending_append_failed", 20)
  assert wait_for_event(log_subject, "scheduled_started_append_failed", 20)
  assert wait_for_event(log_subject, "scheduled_dispatch_started", 20)
  assert wait_for_event(log_subject, "scheduled_worker_exited", 20)
  assert wait_for_event(log_subject, "scheduled_failure_append_failed", 20)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_due_tick_runs_command_workflow_test() {
  let dir = "test/tmp/daemon-scheduled-due"
  let workflow_path = write_scheduled_command_workflow(dir, 1)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: fake_workflow_run_dependencies(command_subject),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(command_subject, "yaml_command:scheduled_command", 20)
  assert wait_for_event(log_subject, "scheduled_worker_exited", 20)
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_succeeded(records, run_id) },
    20,
  )
  let records = load_test_records(root)
  assert has_scheduled_due(records, run_id)
  assert has_scheduled_pending(records, run_id)
  assert has_scheduled_started(records, run_id)
  assert has_scheduled_succeeded(records, run_id)
  assert has_step_success(records, "scheduled_command")
  let projected = load_test_projection(root)
  let assert Ok(status) =
    projection.scheduled_status_for(projected, "scheduled-job")
  assert status.state == projection.ScheduledTerminalSuccess
  assert status.last_success_run_id == Some(run_id)
  let assert Ok(query_types.MetricsResponse(metrics)) =
    daemon.execute_query(started.data, query_types.Metrics, 1000)
  assert metrics.scheduled_job_count == 1
  assert metrics.scheduled_due_count == 0
  assert metrics.scheduled_next_due_count == 1
  assert metrics.running_scheduled_workers == 0
  assert metrics.active_sessions == 0

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_failure_cleans_active_yaml_step_child_test() {
  let dir = "test/tmp/daemon-scheduled-yaml-child-cleanup"
  let workflow_path = write_scheduled_parallel_failure_workflow(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client = empty_tracker_client()
  let log_subject = process.new_subject()
  let worker_barrier = test_async.new_barrier()
  let command_barrier = test_async.new_barrier()
  let assert Ok(event_hub) = hub.start(50, fn() { 42 })
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: scheduled_parallel_failure_workflow_run_dependencies(
        log_subject,
        worker_barrier,
        command_barrier,
      ),
      now_ms: fn() { clock_now(clock) },
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "agent_ready", 20)
  test_async.release_barrier(command_barrier)
  assert wait_for_event(log_subject, "yaml_command_failed:failing_command", 20)
  assert wait_for_event(log_subject, "scheduled_worker_exited", 20)
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_started(records, run_id) },
    20,
  )
  let assert Ok(sessions) = hub.list_sessions(event_hub, 1000)
  let step_sessions =
    list.filter(sessions, fn(summary) {
      string.starts_with(
        summary.session_id,
        "workflow-step-" <> run_id <> "-active_agent-a1-",
      )
    })
  let assert [step_session] = step_sessions
  assert wait_for_session_status(
    event_hub,
    step_session.session_id,
    event.Exited(reason.Stopped),
    20,
  )
  let assert Ok(prompt_result) =
    daemon.apply_operator_command(
      started.data,
      command.PromptSession(step_session.session_id, "after cleanup"),
      1000,
    )
  assert prompt_result.status == command.NotFound
  let assert Ok(metrics) =
    wait_for_metrics(started.data, 20, fn(metrics) {
      metrics.active_sessions == 0 && metrics.running_scheduled_workers == 0
    })
  assert metrics.token_totals.total == 0
  let post_cleanup_logs = test_async.drain_subject(log_subject)
  assert !list.contains(post_cleanup_logs, "agent_survived")
  test_async.release_barrier_if_waiting_within(worker_barrier, 100)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
  hub.stop(event_hub)
}

pub fn daemon_scheduled_startup_clamps_legacy_persisted_due_after_clock_baseline_change_test() {
  let dir = "test/tmp/daemon-scheduled-restart-clock-baseline"
  let workflow_path = write_scheduled_command_workflow(dir, 1)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  append_succeeded_scheduled_run(
    root,
    ms("1951-09-27T03:15:00Z"),
    "schedule-scheduled-job-19510927T031500Z",
  )
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let clock = start_test_clock(ms("2026-05-05T12:00:10Z"))
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: fake_workflow_run_dependencies(command_subject),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  set_clock(clock, ms("2026-05-05T12:00:11Z"))
  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(command_subject, "yaml_command:scheduled_command", 20)
  assert wait_for_event(log_subject, "scheduled_worker_exited", 20)
  let run_id = "schedule-scheduled-job-20260505T120011Z"
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_succeeded(records, run_id) },
    20,
  )
  let records = load_test_records(root)
  assert has_scheduled_due(records, run_id)
  assert has_scheduled_pending(records, run_id)
  assert has_scheduled_started(records, run_id)
  assert has_scheduled_succeeded(records, run_id)
  assert !has_scheduled_skip(records, "catch_up_disabled")

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_overlap_records_skip_without_second_start_test() {
  let dir = "test/tmp/daemon-scheduled-overlap"
  let workflow_path = write_scheduled_command_workflow(dir, 1)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let barrier = test_async.new_barrier()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: blocking_command_workflow_run_dependencies(
        command_subject,
        barrier,
      ),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))
  assert wait_for_event(command_subject, "yaml_command:scheduled_command", 20)
  let assert Ok(active_metrics) =
    wait_for_metrics(started.data, 20, fn(metrics) {
      metrics.running_scheduled_workers == 1
      && metrics.running_workers == 0
      && metrics.active_sessions == 2
    })
  assert active_metrics.running_scheduled_workers == 1
  assert active_metrics.running_workers == 0
  assert active_metrics.active_sessions == 2
  assert active_metrics.scheduled_due_count == 0
  assert active_metrics.scheduled_next_due_count == 1
  let _ = test_async.drain_subject(command_subject)
  let _ = test_async.drain_subject(log_subject)

  set_clock(clock, 2000)
  process.send(started.data, daemon.PollTick(2))
  assert wait_for_event(log_subject, "tick_started", 20)
  test_async.assert_no_extra_message_within(command_subject, 100)
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_skip(records, "overlap_running") },
    20,
  )
  let records = load_test_records(root)
  assert has_scheduled_skip(records, "overlap_running")

  test_async.release_barrier(barrier)
  assert wait_for_event(log_subject, "scheduled_worker_exited", 20)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_capacity_starts_after_issue_failures_park_test() {
  let dir = "test/tmp/daemon-scheduled-capacity-headroom"
  let workflow_path = write_scheduled_capacity_workflow(dir, 4)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let active_a =
    tracker_issue.Issue(..issue("active-a", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let active_b =
    tracker_issue.Issue(..issue("active-b", "ABC-2", "Todo"), labels: [
      "workflow:implementation",
    ])
  let retry_a =
    tracker_issue.Issue(..issue("retry-a", "ABC-3", "Todo"), labels: [
      "workflow:implementation",
    ])
  let retry_b =
    tracker_issue.Issue(..issue("retry-b", "ABC-4", "Todo"), labels: [
      "workflow:implementation",
    ])
  let candidates = [active_a, active_b, retry_a, retry_b]
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok(candidates) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(ids) {
        Ok(
          list.filter(candidates, fn(candidate) {
            list.contains(ids, candidate.id)
          }),
        )
      },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let barrier = test_async.new_barrier()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: retry_failure_and_blocking_command_workflow_run_dependencies(
        command_subject,
        barrier,
      ),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  assert wait_for_events(
    command_subject,
    [
      "issue_command:active-a",
      "issue_command:active-b",
      "issue_command_failed:retry-a",
      "issue_command_failed:retry-b",
    ],
    20,
  )
  assert wait_for_event(log_subject, "issue_parked", 20)
  assert wait_for_event(log_subject, "issue_parked", 20)
  let _ = test_async.drain_subject(command_subject)
  let _ = test_async.drain_subject(log_subject)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(2))
  let assert Ok(_) =
    wait_for_event_prefix(command_subject, "scheduled_command:", 20)
  assert wait_for_records(
    root,
    fn(records) { scheduled_started_count(records) >= 1 },
    20,
  )
  let _ = test_async.drain_subject(log_subject)

  test_async.release_barrier(barrier)
  test_async.release_barrier(barrier)
  test_async.release_barrier(barrier)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_startup_records_active_run_failure_without_retry_test() {
  let dir = "test/tmp/daemon-scheduled-recover-active"
  let workflow_path = write_scheduled_command_workflow(dir, 1)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  append_started_scheduled_run(root, run_id)
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let clock = start_test_clock(1500)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: fake_workflow_run_dependencies(command_subject),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  assert wait_for_records(
    root,
    fn(records) {
      has_scheduled_failed(records, run_id, "daemon_restart", True)
      && !has_scheduled_retry_scheduled(records, run_id, 2)
    },
    20,
  )

  process.send(started.data, daemon.ScheduledRetryTick(run_id, 1))
  test_async.assert_no_extra_message_within(command_subject, 100)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_failure_reports_without_workflow_retry_test() {
  let dir = "test/tmp/daemon-scheduled-report"
  let workflow_path = write_scheduled_reporting_workflow(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let report_subject = process.new_subject()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_tracker_adapter: fn(_) {
        adapter_with_scheduled_reporter(
          client,
          scheduled_reporter_success(report_subject),
        )
      },
      workflow_run_dependencies: failing_command_workflow_run_dependencies(
        command_subject,
      ),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  let assert Ok(_request) = process.receive(report_subject, within: 5000)
  assert wait_for_records(
    root,
    fn(records) {
      has_scheduled_failed_run(records, run_id)
      && has_scheduled_failure_reported(records, run_id, "lin-scheduled")
      && has_scheduled_failure_outbox_completed(records)
      && !has_scheduled_retry_scheduled(records, run_id, 2)
    },
    20,
  )

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_report_retry_does_not_rerun_workflow_test() {
  let dir = "test/tmp/daemon-scheduled-report-retry"
  let workflow_path = write_scheduled_reporting_workflow(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let report_subject = process.new_subject()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_tracker_adapter: fn(_) {
        adapter_with_scheduled_reporter(
          client,
          scheduled_reporter_directed(report_subject),
        )
      },
      workflow_run_dependencies: failing_command_workflow_run_dependencies(
        command_subject,
      ),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  let assert Ok(DirectedScheduledReportCall(first_request, first_reply)) =
    process.receive(report_subject, within: 5000)
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_failure_outbox_attempted(records, 1) },
    20,
  )
  process.send(first_reply, ScheduledReportError)
  assert first_request.run_id == run_id
  assert wait_for_records(
    root,
    fn(records) {
      has_scheduled_failure_report_failed(records, run_id, 1)
      && has_scheduled_failure_outbox_retry(records, 1)
    },
    20,
  )
  let _ = test_async.drain_subject(command_subject)

  process.send(started.data, daemon.ScheduledReportRetryTick(run_id, 2))
  test_async.assert_no_extra_message_within(report_subject, 100)
  test_async.assert_no_extra_message_within(command_subject, 100)

  process.send(started.data, daemon.ScheduledReportRetryTick(run_id, 1))
  let assert Ok(DirectedScheduledReportCall(second_request, second_reply)) =
    process.receive(report_subject, within: 5000)
  process.send(second_reply, ScheduledReportSuccess)
  assert second_request.run_id == run_id
  test_async.assert_no_extra_message_within(command_subject, 100)
  assert wait_for_records(
    root,
    fn(records) {
      has_scheduled_failure_reported(records, run_id, "lin-scheduled")
      && has_scheduled_failure_outbox_completed(records)
    },
    20,
  )

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_report_permanent_failure_does_not_retry_test() {
  let dir = "test/tmp/daemon-scheduled-report-permanent-failure"
  let workflow_path = write_scheduled_reporting_workflow(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let report_subject = process.new_subject()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_tracker_adapter: fn(_) {
        adapter_with_scheduled_reporter(
          client,
          scheduled_reporter_directed(report_subject),
        )
      },
      workflow_run_dependencies: failing_command_workflow_run_dependencies(
        command_subject,
      ),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  let assert Ok(DirectedScheduledReportCall(first_request, first_reply)) =
    process.receive(report_subject, within: 5000)
  process.send(first_reply, ScheduledReportPermanentError)
  assert first_request.run_id == run_id
  assert wait_for_records(
    root,
    fn(records) {
      has_terminal_scheduled_failure_report_failed(records, run_id, 1)
      && has_scheduled_failure_outbox_permanent(records, 1)
    },
    20,
  )

  let assert Ok(snapshot) = daemon.get_read_model_snapshot(started.data, 1000)
  assert snapshot.counts.scheduled_report_retry_count == 0
  assert snapshot.counts.scheduled_report_retry_timer_count == 0
  assert snapshot.counts.permanent_outbox_count == 1
  let _ = test_async.drain_subject(report_subject)
  let _ = test_async.drain_subject(command_subject)
  test_async.assert_no_extra_message_within(command_subject, 100)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_report_retry_stops_after_default_bound_test() {
  let dir = "test/tmp/daemon-scheduled-report-retry-bound"
  let workflow_path = write_scheduled_reporting_workflow(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let report_subject = process.new_subject()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_tracker_adapter: fn(_) {
        adapter_with_scheduled_reporter(
          client,
          scheduled_reporter_directed(report_subject),
        )
      },
      workflow_run_dependencies: failing_command_workflow_run_dependencies(
        command_subject,
      ),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  let assert Ok(DirectedScheduledReportCall(first_request, first_reply)) =
    process.receive(report_subject, within: 5000)
  process.send(first_reply, ScheduledReportError)
  assert first_request.run_id == run_id
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_failure_report_failed(records, run_id, 1) },
    20,
  )

  let max_report_attempts = scheduled_runtime.default_report_max_attempts()
  fail_scheduled_report_retries_until_bound(
    started.data,
    report_subject,
    root,
    run_id,
    1,
    max_report_attempts,
  )

  process.send(
    started.data,
    daemon.ScheduledReportRetryTick(run_id, max_report_attempts),
  )
  test_async.assert_no_extra_message_within(report_subject, 100)
  let assert Ok(snapshot) = daemon.get_read_model_snapshot(started.data, 1000)
  assert snapshot.counts.scheduled_report_retry_count == 0
  assert snapshot.counts.scheduled_report_retry_timer_count == 0
  assert wait_for_records(
    root,
    fn(records) {
      scheduled_failure_report_failed_count(records, run_id)
      == max_report_attempts
    },
    20,
  )

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

fn fail_scheduled_report_retries_until_bound(
  daemon_subject: process.Subject(daemon.Message),
  report_subject: process.Subject(DirectedScheduledReportCall),
  root: String,
  run_id: String,
  failed_generation: Int,
  max_generation: Int,
) -> Nil {
  case failed_generation >= max_generation {
    True -> Nil
    False -> {
      process.send(
        daemon_subject,
        daemon.ScheduledReportRetryTick(run_id, failed_generation),
      )
      let assert Ok(DirectedScheduledReportCall(request, reply)) =
        process.receive(report_subject, within: 5000)
      process.send(reply, ScheduledReportError)
      assert request.run_id == run_id
      let next_generation = failed_generation + 1
      assert wait_for_records(
        root,
        fn(records) {
          case next_generation == max_generation {
            True ->
              has_terminal_scheduled_failure_report_failed(
                records,
                run_id,
                next_generation,
              )
            False ->
              has_scheduled_failure_report_failed(
                records,
                run_id,
                next_generation,
              )
          }
        },
        20,
      )
      fail_scheduled_report_retries_until_bound(
        daemon_subject,
        report_subject,
        root,
        run_id,
        next_generation,
        max_generation,
      )
    }
  }
}

pub fn daemon_scheduled_report_retry_retains_retry_when_outbox_append_fails_test() {
  let dir = "test/tmp/daemon-scheduled-report-retry-outbox-append-fails"
  let workflow_path = write_scheduled_reporting_workflow(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client = empty_tracker_client()
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let report_subject = process.new_subject()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_tracker_adapter: fn(_) {
        adapter_with_scheduled_reporter(
          client,
          scheduled_reporter_directed(report_subject),
        )
      },
      workflow_run_dependencies: failing_command_workflow_run_dependencies(
        command_subject,
      ),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  let assert Ok(DirectedScheduledReportCall(first_request, first_reply)) =
    process.receive(report_subject, within: 5000)
  process.send(first_reply, ScheduledReportError)
  assert first_request.run_id == run_id
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_failure_report_failed(records, run_id, 1) },
    20,
  )

  write_corrupt_current_ledger(root)
  process.send(started.data, daemon.ScheduledReportRetryTick(run_id, 1))

  test_async.assert_no_extra_message_within(report_subject, 100)
  let logs = test_async.drain_subject(log_subject)
  assert list.contains(logs, "outbox_ledger_append_failed")
  assert list.contains(logs, "scheduled_report_retry_retained")
  let assert Ok(snapshot) = daemon.get_read_model_snapshot(started.data, 1000)
  assert snapshot.counts.scheduled_report_retry_count == 1
  assert snapshot.counts.scheduled_report_retry_timer_count == 1
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_report_retry_blocks_new_intervals_until_reported_test() {
  let dir = "test/tmp/daemon-scheduled-report-retry-blocks-intervals"
  let workflow_path = write_scheduled_reporting_workflow(dir)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let report_subject = process.new_subject()
  let clock = start_test_clock(100)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_tracker_adapter: fn(_) {
        adapter_with_scheduled_reporter(
          client,
          scheduled_reporter_directed(report_subject),
        )
      },
      workflow_run_dependencies: failing_command_workflow_run_dependencies(
        command_subject,
      ),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  set_clock(clock, 1000)
  process.send(started.data, daemon.PollTick(1))
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  let assert Ok(DirectedScheduledReportCall(first_request, first_reply)) =
    process.receive(report_subject, within: 5000)
  process.send(first_reply, ScheduledReportError)
  assert first_request.run_id == run_id
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_failure_report_failed(records, run_id, 1) },
    20,
  )
  let _ = test_async.drain_subject(command_subject)

  set_clock(clock, 2000)
  process.send(started.data, daemon.PollTick(2))
  assert wait_for_records(
    root,
    fn(records) { has_scheduled_skip(records, "overlap_running") },
    20,
  )
  test_async.assert_no_extra_message_within(command_subject, 100)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_scheduled_startup_removes_retry_waiting_timer_test() {
  let dir = "test/tmp/daemon-scheduled-recover-retry"
  let workflow_path = write_scheduled_command_workflow(dir, 1)
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let run_id = "schedule-scheduled-job-19700101T000001Z"
  append_retry_waiting_scheduled_run(root, run_id)
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let command_subject = process.new_subject()
  let clock = start_test_clock(1500)
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: fake_workflow_run_dependencies(command_subject),
      now_ms: fn() { clock_now(clock) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  assert wait_for_records(
    root,
    fn(records) {
      has_scheduled_failed(records, run_id, "workflow_command_failed", True)
    },
    20,
  )
  process.send(started.data, daemon.ScheduledRetryTick(run_id, 1))
  test_async.assert_no_extra_message_within(command_subject, 100)

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
  process.send(clock, StopClock)
}

pub fn daemon_yaml_poll_dispatches_command_workflow_test() {
  let dir = "test/tmp/daemon-yaml"
  let assert Ok(marker) = path.absolute(dir <> "/marker")
  let workflow_path = write_yaml_workflow(dir, marker)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "yaml_command:final_test", 20)
  assert wait_for_event(log_subject, "worker_exited", 20)
  assert simplifile.is_file(marker) != Ok(True)
  let assert Ok(sessions) = hub.list_sessions(event_hub, 1000)
  let session_ids = list.map(sessions, fn(summary) { summary.session_id })
  assert list.contains(
    session_ids,
    "workflow-step-ABC-1-42-1-final_test-a1-c55a07a40185",
  )
  let identity = orchestrator_state.issue_identity(candidate)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(snapshot.completed, identity)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_command_failure_diagnostics_reach_events_and_report_test() {
  let dir = "test/tmp/daemon-command-failure-diagnostics"
  let workflow_path = write_real_failing_command_workflow(dir)
  let candidate =
    tracker_issue.Issue(..issue("issue-id", "ABC-1", "Todo"), labels: [
      "workflow:implementation",
    ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let failure_report_subject = process.new_subject()
  let assert Ok(event_hub) = hub.start(20, fn() { 42 })
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_tracker_adapter: fn(_) {
        adapter_with_handoff_failure_subject(client, failure_report_subject)
      },
      workflow_run_dependencies: workflow_run.default_dependencies(),
      start_event_hub: fn() { Ok(event_hub) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))

  let assert Ok(_) = wait_for_event_result(log_subject, "worker_exited", 20)
  let step_session_id = "workflow-step-ABC-1-42-1-final_test-a1-c55a07a40185"
  let assert Ok(page) =
    hub.events_after(event_hub, step_session_id, 0, 20, 1000)
  let assert Some(command_event) = find_event(page.events, "command_failed")
  assert event.payload_kind(command_event.payload) == event.Error
  let assert Some(message) = event.payload_message(command_event.payload)
  assert string.contains(message, "step=final_test")
  assert string.contains(message, "exit_code=9")
  assert string.contains(message, "stdout=")
  assert string.contains(message, "stderr=")
  assert string.contains(message, "[truncated]")
  let assert Some(tool_output) =
    event.payload_tool_output(command_event.payload)
  assert string.contains(tool_output, "full retained artifact:")
  assert string.contains(tool_output, "stdout_truncated: true")
  assert string.contains(tool_output, "stderr_truncated: true")

  let failure_comment =
    test_async.expect_message_within(failure_report_subject, 5000)
  assert string.contains(failure_comment, "workflow_step_failed")
  assert string.contains(failure_comment, "step=final_test")
  assert string.contains(failure_comment, "exit_code=9")
  assert string.contains(failure_comment, "artifact=")
  assert string.contains(failure_comment, "stdout=")
  assert string.contains(failure_comment, "stderr=")
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_poll_dispatches_fake_worker_routes_update_and_shutdown_test() {
  let workflow_path = write_workflow("test/tmp/daemon-basic", 1)
  let candidate = issue("issue-id", "ABC-1", "Todo")
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([candidate]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let assert Ok(started) =
    daemon.start(Some(workflow_path), base_dependencies(client, log_subject))
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))

  assert wait_for_event(log_subject, "tick_started", 10)
  assert wait_for_event(log_subject, "dispatch_started", 10)
  assert wait_for_event(log_subject, "pi_event", 10)
  assert wait_for_event(log_subject, "worker_exited", 10)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_side_effect_crash_does_not_stall_future_polls_test() {
  use <- expected_crash.suppressing([
    "test/orchestrator_daemon_test.gleam",
    "daemon_side_effect_crash_does_not_stall_future_polls_test",
    "candidate fetch crashed",
  ])
  let workflow_path = write_workflow("test/tmp/daemon-side-effect-crash", 1)
  let fetch_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() {
        let reply = process.new_subject()
        process.send(fetch_subject, FetchRequest(reply))
        case process.receive(reply, within: 1000) {
          Ok(CrashFetch) -> panic as "candidate fetch crashed"
          Ok(ReturnCandidates(candidates)) -> Ok(candidates)
          Error(_) -> Error(error.LinearApiRequest("fetch directive timeout"))
        }
      },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([]) },
    )
  let log_subject = process.new_subject()
  let assert Ok(started) =
    daemon.start(Some(workflow_path), base_dependencies(client, log_subject))
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(FetchRequest(first_reply)) =
    process.receive(fetch_subject, within: 1000)
  process.send(first_reply, CrashFetch)
  assert wait_for_event(log_subject, "side_effect_crashed", 20)

  process.send(started.data, daemon.PollTick(2))
  let assert Ok(FetchRequest(second_reply)) =
    process.receive(fetch_subject, within: 1000)
  process.send(second_reply, ReturnCandidates([]))

  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_failed_issue_parks_without_rescheduling_test() {
  let workflow_path = write_workflow("test/tmp/daemon-retry-terminal", 1)
  let retried = issue("retry-id", "ABC-2", "Todo")
  let log_subject = process.new_subject()
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([retried]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        let reply = process.new_subject()
        process.send(refresh_subject, reply)
        case process.receive(reply, within: 1000) {
          Ok(issue) -> Ok([issue])
          Error(_) -> Error(error.LinearApiRequest("refresh timeout"))
        }
      },
    )
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: workflow_run.Dependencies(
        ..fake_workflow_run_dependencies(log_subject),
        agent_step: fn(
          issue: tracker_issue.Issue,
          context: workflow_run.StepContext,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
        ) {
          let _ = issue
          Error(agent_types.WorkerFailure(
            reason: error.PiFailed(error.PiProtocolError("boom")),
            workspace_path: Some(context.workspace_path),
            tokens: session_tokens.zero_token_totals(),
            final_issue: None,
          ))
        },
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(initial_refresh) =
    process.receive(refresh_subject, within: 1000)
  process.send(initial_refresh, retried)
  assert wait_for_event(log_subject, "issue_parked", 20)

  let identity = orchestrator_state.issue_identity(retried)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(snapshot.parked, identity)
  assert !dict.has_key(snapshot.retry_attempts, identity)
  test_async.assert_no_extra_message_within(refresh_subject, 100)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_failed_worker_runs_once_without_retry_timer_test() {
  let workflow_path = write_workflow("test/tmp/daemon-retry", 1)
  let first = issue("retry-id", "ABC-2", "Todo")
  let log_subject = process.new_subject()
  let refresh_subject = process.new_subject()
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([first]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) {
        let reply = process.new_subject()
        process.send(refresh_subject, reply)
        case process.receive(reply, within: 1000) {
          Ok(issue) -> Ok([issue])
          Error(_) -> Error(error.LinearApiRequest("refresh timeout"))
        }
      },
    )
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: workflow_run.Dependencies(
        ..fake_workflow_run_dependencies(log_subject),
        agent_step: fn(
          _issue: tracker_issue.Issue,
          context: workflow_run.StepContext,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
        ) {
          process.send(log_subject, "agent_run")
          Error(agent_types.WorkerFailure(
            reason: error.PiFailed(error.PiProtocolError("boom")),
            workspace_path: Some(context.workspace_path),
            tokens: session_tokens.zero_token_totals(),
            final_issue: None,
          ))
        },
      ),
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  process.send(started.data, daemon.PollTick(1))
  let assert Ok(initial_refresh) =
    process.receive(refresh_subject, within: 1000)
  process.send(initial_refresh, first)
  assert wait_for_event(log_subject, "issue_parked", 20)
  test_async.assert_no_extra_message_within(refresh_subject, 100)

  let identity = orchestrator_state.issue_identity(first)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(snapshot.parked, identity)
  assert !dict.has_key(snapshot.retry_attempts, identity)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_startup_parks_interrupted_run_without_retry_generation_test() {
  let dir = "test/tmp/daemon-startup-interrupted-retry"
  let workflow_path = write_workflow(dir, 1)
  let assert Ok(bundle) = runtime_bundle.load(Some(workflow_path))
  let candidate = issue("retry-id", "ABC-RETRY", "Todo")
  let workspace_root = bundle.effective.workspace.root
  append_test_ledger_bodies(workspace_root, [
    record.RunStarted(
      run_id: "run-1",
      issue_id: candidate.id,
      issue_identifier: candidate.identifier,
      workspace_path: workspace_root <> "/ABC-RETRY",
    ),
  ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(ids) {
        Ok(list.filter([candidate], fn(issue) { list.contains(ids, issue.id) }))
      },
    )
  let log_subject = process.new_subject()
  let deps = base_dependencies(client, log_subject)
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  let identity = orchestrator_state.issue_identity(candidate)
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  assert dict.has_key(snapshot.parked, identity)
  assert !dict.has_key(snapshot.retry_attempts, identity)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_startup_cleanup_workspace_for_terminal_interrupted_run_test() {
  let dir = "test/tmp/daemon-startup-cleanup"
  let workflow_path = write_workflow(dir, 1)
  let assert Ok(bundle) = runtime_bundle.load(Some(workflow_path))
  let candidate = issue("cleanup-id", "ABC-CLEAN", "Done")
  let workspace_root = bundle.effective.workspace.root
  let workspace_path = workspace_root <> "/ABC-CLEAN"
  append_test_ledger_bodies(workspace_root, [
    record.RunStarted(
      run_id: "run-1",
      issue_id: candidate.id,
      issue_identifier: candidate.identifier,
      workspace_path: workspace_path,
    ),
  ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(ids) {
        Ok(list.filter([candidate], fn(issue) { list.contains(ids, issue.id) }))
      },
    )
  let log_subject = process.new_subject()
  let cleanup_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      cleanup: fn(_, workspace_path, _) {
        process.send(cleanup_subject, workspace_path)
        Ok(Nil)
      },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  assert wait_for_event(log_subject, "recovered_workspace_cleanup", 20)
  assert process.receive(cleanup_subject, within: 1000) == Ok(workspace_path)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_startup_identity_mismatch_parks_without_resuming_test() {
  let dir = "test/tmp/daemon-startup-identity-mismatch"
  let workflow_path = write_workflow(dir, 1)
  let assert Ok(bundle) = runtime_bundle.load(Some(workflow_path))
  let original_issue = issue("issue-park", "ABC-PARK", "Todo")
  let changed_issue =
    tracker_issue.Issue(..original_issue, title: "Changed title ABC-PARK")
  let workspace_root = bundle.effective.workspace.root
  let run_root = workspace_root <> "/implementation/ABC-PARK/run-1"
  let assert Ok(#(_, dag)) =
    runtime_bundle.select_workflow(bundle, original_issue)
  let assert Ok(fingerprint) =
    workflow_fingerprint.fingerprint_for_execution(dag, bundle.orchestrator)
  append_test_ledger_bodies(workspace_root, [
    record.WorkflowRunStarted(
      "run-1",
      "implementation",
      fingerprint,
      original_issue.id,
      original_issue.identifier,
      core.issue_fingerprint(original_issue),
      0,
      run_root,
    ),
  ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(ids) {
        Ok(
          list.filter([changed_issue], fn(issue) {
            list.contains(ids, issue.id)
          }),
        )
      },
    )
  let log_subject = process.new_subject()
  let park_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      make_tracker_adapter: fn(_) {
        adapter_with_park_report_subject(client, park_subject)
      },
      logger: fn(_, _, _, _) { Ok(Nil) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  let assert Ok(#(issue_id, reason, Some(run_id))) =
    process.receive(park_subject, within: 1000)
  assert issue_id == "issue-park"
  assert string.starts_with(reason, "issue_content_drift:")
  assert run_id == "run-1"
  let assert Ok(snapshot) = daemon.get_snapshot(started.data, 1000)
  let identity = orchestrator_state.issue_identity(changed_issue)
  assert dict.has_key(snapshot.parked, identity)
  assert !dict.has_key(snapshot.running, identity)
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String

pub fn daemon_startup_resumes_matching_workflow_checkpoint_test() {
  let dir = "test/tmp/daemon-startup-resume-workflow"
  test_helpers.reset_dir(dir)
  let config_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let assert Ok(workspace_root) = path.absolute(dir <> "/workspaces")
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(Nil) =
    simplifile.write(config_path, workflow_text(workspace_root, 1))
  let assert Ok(Nil) =
    simplifile.write(
      workflow_dir <> "/implementation.yaml",
      "version: 1
id: implementation
steps:
  - id: first
    kind: command
    run: first
    run_in: main
  - id: second
    kind: command
    depends_on: [first]
    run: second
    run_in: main
",
    )
  let candidate = issue("issue-resume", "ABC-99", "Todo")
  let assert Ok(bundle) = runtime_bundle.load(Some(config_path))
  let assert Ok(#(_, dag)) = runtime_bundle.select_workflow(bundle, candidate)
  let assert Ok(fingerprint) =
    workflow_fingerprint.fingerprint_for_execution(dag, bundle.orchestrator)
  let store = artifact_store.new(workspace_root)
  let completed_artifact =
    step_artifact.from_command_result(
      "first",
      0,
      "already done",
      "",
      False,
      [],
      bundle.orchestrator.artifact_limits,
    )
  let assert Ok(stored) =
    artifact_store.write_step_artifact(
      store,
      "run-recover",
      "implementation",
      "first",
      1,
      completed_artifact,
    )
  let run_root = workspace_root <> "/implementation/ABC-99/run-recover"
  let first_workspace = run_root <> "/main"
  let assert Ok(Nil) = simplifile.create_directory_all(first_workspace)
  let assert Ok(ledger_path) = ledger.path_for_workspace_root(workspace_root)
  let assert Ok(Nil) =
    ledger.append_many(
      ledger_path,
      [
        record.with_id(
          "workflow-started",
          1,
          record.WorkflowRunStarted(
            "run-recover",
            "implementation",
            fingerprint,
            candidate.id,
            candidate.identifier,
            core.issue_fingerprint(candidate),
            0,
            run_root,
          ),
        ),
        record.with_id(
          "first-prepared",
          2,
          record.StepAttemptPrepared(
            "run-recover",
            "implementation",
            "first",
            1,
            "main",
            first_workspace,
            run_root,
            None,
            None,
          ),
        ),
        record.with_id(
          "first-started",
          3,
          record.StepAttemptStarted(
            "run-recover",
            "implementation",
            "first",
            1,
            "workflow-step-run-recover-first-a1-a7937b64b8ca",
            None,
            False,
          ),
        ),
        record.with_id(
          "first-finished",
          4,
          record.StepAttemptFinished(
            "run-recover",
            "implementation",
            "first",
            1,
            "completed",
            stored.ref,
            stored.sha256,
            "main",
            first_workspace,
            0,
            0,
          ),
        ),
      ],
      True,
    )
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      logger: fn(_, _, _, _) { Ok(Nil) },
    )
  let assert Ok(started) = daemon.start(Some(config_path), deps)

  assert wait_for_event_without_event(
    log_subject,
    "yaml_command:second",
    "yaml_command:first",
    10,
  )
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_startup_resume_preserves_recovered_success_outcome_test() {
  let dir = "test/tmp/daemon-startup-recovered-success"
  let workflow_path = write_yaml_agent_workflow(dir)
  let assert Ok(bundle) = runtime_bundle.load(Some(workflow_path))
  let candidate =
    tracker_issue.Issue(
      ..issue("issue-recovered-success", "ABC-100", "Todo"),
      labels: [
        "workflow:implementation",
      ],
    )
  let assert Ok(#(_, dag)) = runtime_bundle.select_workflow(bundle, candidate)
  let assert Ok(fingerprint) =
    workflow_fingerprint.fingerprint_for_execution(dag, bundle.orchestrator)
  let workspace_root = bundle.effective.workspace.root
  let run_root = workspace_root <> "/implementation/ABC-100/run-recovered"
  append_test_ledger_bodies(workspace_root, [
    record.WorkflowRunStarted(
      "run-recovered",
      "implementation",
      fingerprint,
      candidate.id,
      candidate.identifier,
      core.issue_fingerprint(candidate),
      0,
      run_root,
    ),
    record.WorkflowStepRecoveryStarted(
      "run-recovered",
      "implementation",
      "implement",
      1,
      1,
      "recovery-session-1",
      Some("test-model"),
      "artifacts://prompt.md",
    ),
  ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      logger: fn(_, _, _, _) { Ok(Nil) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  assert wait_for_workflow_finished_outcomes(
    workspace_root,
    ["succeeded_after_recovery"],
    20,
  )
  assert workflow_finished_task_refs(workspace_root)
    == [
      record.linear_task_ref_fields(
        candidate.id,
        Some(candidate.identifier),
        None,
      ),
    ]
  assert legacy_terminal_record_kinds(workspace_root) == []
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}

pub fn daemon_startup_resume_preserves_recovered_failure_outcome_test() {
  let dir = "test/tmp/daemon-startup-recovered-failure"
  let workflow_path = write_yaml_agent_workflow(dir)
  let assert Ok(bundle) = runtime_bundle.load(Some(workflow_path))
  let candidate =
    tracker_issue.Issue(
      ..issue("issue-recovered-failure", "ABC-101", "Todo"),
      labels: [
        "workflow:implementation",
      ],
    )
  let assert Ok(#(_, dag)) = runtime_bundle.select_workflow(bundle, candidate)
  let assert Ok(fingerprint) =
    workflow_fingerprint.fingerprint_for_execution(dag, bundle.orchestrator)
  let workspace_root = bundle.effective.workspace.root
  let run_root = workspace_root <> "/implementation/ABC-101/run-recovered"
  append_test_ledger_bodies(workspace_root, [
    record.WorkflowRunStarted(
      "run-recovered",
      "implementation",
      fingerprint,
      candidate.id,
      candidate.identifier,
      core.issue_fingerprint(candidate),
      0,
      run_root,
    ),
    record.WorkflowStepRecoveryStarted(
      "run-recovered",
      "implementation",
      "implement",
      1,
      1,
      "recovery-session-1",
      Some("test-model"),
      "artifacts://prompt.md",
    ),
    record.WorkflowStepRecoveryFinished(
      "run-recovered",
      "implementation",
      "implement",
      1,
      1,
      "recovery-session-1",
      "recheck",
      "summary",
      "reason",
      Some(2),
    ),
  ])
  let client =
    tracker.Client(
      fetch_candidate_issues: fn() { Ok([]) },
      fetch_issues_by_states: fn(_) { Ok([]) },
      fetch_issue_states_by_ids: fn(_) { Ok([candidate]) },
    )
  let log_subject = process.new_subject()
  let deps =
    daemon.RuntimeDependencies(
      ..base_dependencies(client, log_subject),
      workflow_run_dependencies: failing_agent_workflow_run_dependencies(
        log_subject,
      ),
      logger: fn(_, _, _, _) { Ok(Nil) },
    )
  let assert Ok(started) = daemon.start(Some(workflow_path), deps)
  wait_until_startup_recovery_ready(started.data)

  assert wait_for_workflow_finished_outcomes(
    workspace_root,
    ["failed_after_recovery"],
    20,
  )
  assert workflow_finished_task_refs(workspace_root)
    == [
      record.linear_task_ref_fields(
        candidate.id,
        Some(candidate.identifier),
        None,
      ),
    ]
  assert legacy_terminal_record_kinds(workspace_root) == []
  assert daemon.shutdown(started.data, 1000) == Ok(Nil)
}
