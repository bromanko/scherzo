import gleam/option.{None, Some}
import gleam/string
import scherzo/instance_lock
import scherzo/managed_launch/status
import scherzo/orchestrator/daemon
import scherzo/orchestrator/service
import scherzo/path
import scherzo/signal
import simplifile
import support/test_helpers

pub fn managed_launch_writes_grant_validation_status_before_startup_test() {
  let root = "test/tmp/orchestrator-service-managed-launch-invalid-grant"
  test_helpers.reset_dir(root)
  let assert Ok(abs_root) = path.absolute(root)
  let config_path = root <> "/scherzo.yaml"
  let workflow_dir = root <> "/workflows"
  let workflow_path = workflow_dir <> "/implementation.yaml"
  let grant_path = root <> "/grant.json"
  let status_path = root <> "/status.json"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(Nil) = simplifile.write(workflow_path, workflow_yaml())
  let assert Ok(Nil) = simplifile.write(config_path, yaml_config(abs_root))
  let assert Ok(Nil) = simplifile.write(grant_path, invalid_grant_json)
  let assert Ok(Nil) = simplifile.set_permissions_octal(root, 0o700)
  let assert Ok(Nil) = simplifile.set_permissions_octal(grant_path, 0o600)

  let result =
    service.start_daemon_with_lifecycle(
      service.DaemonStartOptions(
        workflow_path: Some(config_path),
        managed_launch: Some(service.ManagedLaunchFiles(grant_path, status_path)),
      ),
      service.DaemonLifecycleDependencies(
        daemon_dependencies: daemon.default_dependencies(),
        install_stop_source: fn(_) { Error(signal.InstallFailed("unused")) },
        shutdown_timeout_ms: 10_000,
        lifecycle_logger: fn(_, _, _) { Nil },
      ),
    )

  let assert Error(service.StartupError(code, _)) = result
  assert code == "missing_state_capability"
  let assert Ok(contents) = simplifile.read(status_path)
  let assert Ok(saved) = status.decode_string(contents)
  assert saved.launch_id == None
  assert saved.phase == "grant_validation"
  assert saved.ok == False
  assert saved.code == "missing_state_capability"
  assert !string.contains(contents, "launch_secret_missing_state")
}

pub fn managed_launch_writes_instance_lock_status_before_startup_test() {
  let root = "test/tmp/orchestrator-service-managed-launch"
  test_helpers.reset_dir(root)
  let assert Ok(abs_root) = path.absolute(root)
  let config_path = root <> "/scherzo.yaml"
  let workflow_dir = root <> "/workflows"
  let workflow_path = workflow_dir <> "/implementation.yaml"
  let grant_path = root <> "/grant.json"
  let status_path = root <> "/status.json"
  let assert Ok(Nil) = simplifile.create_directory_all(workflow_dir)
  let assert Ok(Nil) = simplifile.write(workflow_path, workflow_yaml())
  let assert Ok(Nil) = simplifile.write(config_path, yaml_config(abs_root))
  let assert Ok(Nil) = simplifile.write(grant_path, grant_json)
  let assert Ok(Nil) = simplifile.set_permissions_octal(root, 0o700)
  let assert Ok(Nil) = simplifile.set_permissions_octal(grant_path, 0o600)

  let assert Ok(lock) = instance_lock.acquire(abs_root)
  let result =
    service.start_daemon_with_lifecycle(
      service.DaemonStartOptions(
        workflow_path: Some(config_path),
        managed_launch: Some(service.ManagedLaunchFiles(grant_path, status_path)),
      ),
      service.DaemonLifecycleDependencies(
        daemon_dependencies: daemon.default_dependencies(),
        install_stop_source: fn(_) { Error(signal.InstallFailed("unused")) },
        shutdown_timeout_ms: 10_000,
        lifecycle_logger: fn(_, _, _) { Nil },
      ),
    )
  instance_lock.release(lock)

  let assert Error(service.StartupError(code, _)) = result
  assert code == "instance_lock_held"
  let assert Ok(contents) = simplifile.read(status_path)
  let assert Ok(saved) = status.decode_string(contents)
  assert saved.ok == False
  assert saved.code == "instance_lock_held"
}

fn workflow_yaml() -> String {
  "version: 1\nid: implementation\nsteps:\n  - id: noop\n    kind: command\n    run: /usr/bin/true\n    run_in: main\n"
}

fn yaml_config(root: String) -> String {
  "version: 1\ntracker:\n  linear:\n    api_key_env: HOME\n    project: TEST\n  states:\n    ready: [Todo]\n    active: [Todo]\n    terminal: [Done]\nworkspace:\n  root: "
  <> root
  <> "\nagents:\n  concurrency: 0\n  sessions_per_task: 1\n  runtime:\n    type: pi\n    pi:\n      executable: fake\ntask_routing:\n  labels:\n    require_exactly_one: false\n    default_workflow: implementation\nworkflows:\n  implementation: workflows/implementation.yaml\n"
}

const grant_json = "{\"version\":1,\"launchId\":\"launch-123\",\"endpoint\":\"https://ui.example.test\",\"credential\":\"launch_secret_1\",\"capabilities\":[\"state\"],\"commandBridgeEnabled\":false,\"expiresAt\":\"2999-01-01T00:00:00Z\"}"

const invalid_grant_json = "{\"version\":1,\"launchId\":\"launch-123\",\"endpoint\":\"https://ui.example.test\",\"credential\":\"launch_secret_missing_state\",\"capabilities\":[\"query\"],\"commandBridgeEnabled\":false,\"expiresAt\":\"2999-01-01T00:00:00Z\"}"
