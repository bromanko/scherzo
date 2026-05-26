import gleam/erlang/process
import gleam/option.{Some}
import gleam/string
import scherzo/instance_lock
import scherzo/lifecycle
import scherzo/log
import scherzo/orchestrator/daemon
import scherzo/orchestrator/service
import scherzo/path
import scherzo/session/hub
import scherzo/signal
import scherzo/tracker
import scherzo/tracker/adapter_legacy
import simplifile
import support/test_helpers
import test_async

fn workflow_text(root: String) -> String {
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
polling:
  interval_ms: 1000
agent:
  max_concurrent_agents: 0
  max_retry_attempts: 1
  max_sessions_per_issue: 1
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
  test_helpers.reset_dir(dir)
  let workflow_path = dir <> "/scherzo.yaml"
  let workflow_dir = dir <> "/workflows"
  let prompt_dir = workflow_dir <> "/prompts"
  let assert Ok(root) = path.absolute(dir <> "/workspaces")
  let assert Ok(Nil) = simplifile.create_directory_all(prompt_dir)
  let assert Ok(Nil) = simplifile.write(workflow_path, workflow_text(root))
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
  #(workflow_path, root)
}

fn daemon_dependencies(
  log_subject: process.Subject(String),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..daemon.default_dependencies(),
    make_tracker_adapter: fn(_) {
      adapter_legacy.adapter_from_legacy_client(empty_tracker(), "linear")
    },
    cleanup: fn(_, _, _) { Ok(Nil) },
    logger: fn(_, event, fields, _) {
      process.send(log_subject, daemon_log_value(event, fields))
      Ok(Nil)
    },
    now_ms: fn() { 42 },
    send_after: fn(_, delay, _) { daemon.TestTimer(delay) },
    cancel_timer: fn(_) { Nil },
  )
}

fn no_control_dependencies(
  log_subject: process.Subject(String),
) -> daemon.RuntimeDependencies {
  daemon.RuntimeDependencies(
    ..daemon_dependencies(log_subject),
    start_control_server: fn(_, _) { Ok(daemon.NoControlServer) },
    stop_control_server: fn(_) { Nil },
  )
}

fn lifecycle_dependencies(
  daemon_dependencies: daemon.RuntimeDependencies,
  install_stop_source: fn(process.Subject(lifecycle.StopReason)) ->
    Result(signal.Installation, signal.SignalError),
  shutdown_timeout_ms: Int,
  log_subject: process.Subject(String),
) -> service.DaemonLifecycleDependencies {
  service.DaemonLifecycleDependencies(
    daemon_dependencies: daemon_dependencies,
    install_stop_source: install_stop_source,
    shutdown_timeout_ms: shutdown_timeout_ms,
    lifecycle_logger: fn(_, event, fields) {
      process.send(log_subject, lifecycle_log_value(event, fields))
    },
  )
}

fn fake_install(
  ready: process.Subject(process.Subject(lifecycle.StopReason)),
  cleanup_subject: process.Subject(String),
) -> fn(process.Subject(lifecycle.StopReason)) ->
  Result(signal.Installation, signal.SignalError) {
  fn(stop_subject) {
    process.send(ready, stop_subject)
    Ok(signal.Installation(
      cleanup: fn() { process.send(cleanup_subject, "cleanup") },
      installed_signals: [lifecycle.Sigterm],
      os_pid: "test-pid",
    ))
  }
}

pub fn start_daemon_releases_lock_when_signal_install_fails_test() {
  let #(workflow_path, root) =
    write_workflow("test/tmp/service-lifecycle-install-fails")
  let log_subject = process.new_subject()
  let deps =
    lifecycle_dependencies(
      no_control_dependencies(log_subject),
      fn(_) { Error(signal.InstallFailed("boom")) },
      1000,
      log_subject,
    )

  let assert Error(err) =
    service.start_daemon_with_lifecycle(Some(workflow_path), deps)
  assert err.code == "signal_handler_failed"
  assert err.message == "boom"
  let assert Ok(lock) = instance_lock.acquire(root)
  instance_lock.release(lock)
}

pub fn start_daemon_cleans_signal_and_releases_lock_when_daemon_start_fails_test() {
  let #(workflow_path, root) =
    write_workflow("test/tmp/service-lifecycle-daemon-start-fails")
  let log_subject = process.new_subject()
  let cleanup_subject = process.new_subject()
  let daemon_deps =
    daemon.RuntimeDependencies(
      ..no_control_dependencies(log_subject),
      start_event_hub: fn() { Error(hub.InvalidLimit(0)) },
    )
  let deps =
    lifecycle_dependencies(
      daemon_deps,
      fn(_) {
        Ok(signal.Installation(
          cleanup: fn() { process.send(cleanup_subject, "cleanup") },
          installed_signals: [lifecycle.Sigterm],
          os_pid: "test-pid",
        ))
      },
      1000,
      log_subject,
    )

  let assert Error(_) =
    service.start_daemon_with_lifecycle(Some(workflow_path), deps)
  assert process.receive(cleanup_subject, within: 1000) == Ok("cleanup")
  test_async.assert_no_extra_message_within(cleanup_subject, 50)
  let assert Ok(lock) = instance_lock.acquire(root)
  instance_lock.release(lock)
}

pub fn graceful_service_stop_removes_control_file_and_releases_lock_test() {
  let #(workflow_path, root) = write_workflow("test/tmp/service-lifecycle-stop")
  let log_subject = process.new_subject()
  let ready = process.new_subject()
  let cleanup_subject = process.new_subject()
  let result_subject = process.new_subject()
  let deps =
    lifecycle_dependencies(
      daemon_dependencies(log_subject),
      fake_install(ready, cleanup_subject),
      1000,
      log_subject,
    )

  let _pid =
    process.spawn_unlinked(fn() {
      let result =
        service.start_daemon_with_lifecycle(Some(workflow_path), deps)
      process.send(result_subject, result)
    })

  let assert Ok(stop_subject) = process.receive(ready, within: 1000)
  let assert Ok(control_line) =
    wait_for_prefixed_log(log_subject, "control_file:", 20)
  let control_path =
    string.drop_start(control_line, string.length("control_file:"))
  let assert Error(instance_lock.LockAlreadyHeld(_)) =
    instance_lock.acquire(root)

  process.send(stop_subject, lifecycle.TestStop("service-test"))

  let assert Ok(Ok(Nil)) = process.receive(result_subject, within: 5000)
  assert process.receive(cleanup_subject, within: 1000) == Ok("cleanup")
  test_async.assert_no_extra_message_within(cleanup_subject, 50)
  assert wait_for_log(log_subject, "daemon_shutdown", 20)
  assert wait_for_log(log_subject, "daemon_shutdown_complete", 20)
  assert simplifile.is_file(control_path) != Ok(True)
  let assert Ok(lock) = instance_lock.acquire(root)
  instance_lock.release(lock)
}

pub fn daemon_shutdown_timeout_returns_error_and_releases_lock_test() {
  let #(workflow_path, root) =
    write_workflow("test/tmp/service-lifecycle-timeout")
  let log_subject = process.new_subject()
  let ready = process.new_subject()
  let cleanup_subject = process.new_subject()
  let result_subject = process.new_subject()
  let deps =
    lifecycle_dependencies(
      no_control_dependencies(log_subject),
      fake_install(ready, cleanup_subject),
      0,
      log_subject,
    )

  let _pid =
    process.spawn_unlinked(fn() {
      let result =
        service.start_daemon_with_lifecycle(Some(workflow_path), deps)
      process.send(result_subject, result)
    })

  let assert Ok(stop_subject) = process.receive(ready, within: 1000)
  process.send(stop_subject, lifecycle.TestStop("timeout"))

  let assert Ok(Error(err)) = process.receive(result_subject, within: 5000)
  assert err.code == "daemon_shutdown_timeout"
  assert process.receive(cleanup_subject, within: 1000) == Ok("cleanup")
  test_async.assert_no_extra_message_within(cleanup_subject, 50)
  assert wait_for_log(log_subject, "daemon_shutdown_timeout", 20)
  let assert Ok(lock) = instance_lock.acquire(root)
  instance_lock.release(lock)
}

fn empty_tracker() -> tracker.Client {
  tracker.Client(
    fetch_candidate_issues: fn() { Ok([]) },
    fetch_issues_by_states: fn(_) { Ok([]) },
    fetch_issue_states_by_ids: fn(_) { Ok([]) },
  )
}

fn daemon_log_value(event: String, fields: List(log.Field)) -> String {
  case event == "control_server_started" {
    True -> "control_file:" <> find_field(fields, "control_file")
    False -> event
  }
}

fn lifecycle_log_value(event: String, fields: List(log.Field)) -> String {
  case event == "signal_handler_installed" {
    True -> "signal_handler_installed:" <> find_field(fields, "os_pid")
    False -> event
  }
}

fn find_field(fields: List(log.Field), key: String) -> String {
  case fields {
    [] -> ""
    [#(field_key, value), ..rest] ->
      case field_key == key {
        True -> value
        False -> find_field(rest, key)
      }
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

fn wait_for_prefixed_log(
  subject: process.Subject(String),
  prefix: String,
  attempts: Int,
) -> Result(String, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case process.receive(subject, within: 100) {
        Ok(actual) ->
          case string.starts_with(actual, prefix) {
            True -> Ok(actual)
            False -> wait_for_prefixed_log(subject, prefix, attempts - 1)
          }
        Error(_) -> wait_for_prefixed_log(subject, prefix, attempts - 1)
      }
  }
}
