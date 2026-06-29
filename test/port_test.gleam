import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import scherzo/path as scherzo_path
import scherzo/port
import simplifile
import support/test_helpers
import test_async

pub fn port_cwd_and_stdin_stdout_test() {
  let cwd = "test/tmp/port-cwd"
  test_helpers.reset_dir(cwd)

  let assert Ok(process) =
    port.start("pwd; while IFS= read -r line; do echo \"$line\"; done", cwd)
  let pwd_result = port.read_stdout_line(process, 1000)
  let send_result = port.send_line(process, "hello")
  let line_result = port.read_stdout_line(process, 1000)
  let terminate_result = port.terminate(process)

  let assert Ok(pwd) = pwd_result
  assert string.ends_with(pwd, "/" <> cwd)
  let assert Ok(Nil) = send_result
  let assert Ok(line) = line_result
  assert line == "hello"
  let assert Ok(Nil) = terminate_result
}

pub fn port_keeps_stderr_out_of_stdout_test() {
  let cwd = "test/tmp/port-stderr"
  test_helpers.reset_dir(cwd)

  let assert Ok(process) =
    port.start("echo '{\"ok\":true}'; echo diagnostic >&2", cwd)
  let assert Ok(stdout) = port.read_stdout_line(process, 1000)
  assert stdout == "{\"ok\":true}"
  let assert Error(port.ProcessExited(0)) = port.read_stdout_line(process, 1000)
  let assert Ok(diagnostics) = port.read_diagnostics(process)
  let _ = port.terminate(process)
  assert string.contains(diagnostics, "diagnostic")
  assert !string.contains(stdout, "diagnostic")
}

pub fn port_start_with_env_applies_environment_test() {
  let cwd = "test/tmp/port-env"
  test_helpers.reset_dir(cwd)

  let assert Ok(process) =
    port.start_with_env("printf '%s\n' \"$SCHERZO_TEST_ENV\"", cwd, [
      #("SCHERZO_TEST_ENV", "hello from env"),
    ])
  let assert Ok(stdout) = port.read_stdout_line(process, 1000)
  assert stdout == "hello from env"
  let assert Error(port.ProcessExited(0)) = port.read_stdout_line(process, 1000)
  let _ = port.terminate(process)
}

pub fn port_start_preserves_current_path_across_shell_rewrites_test() {
  let cwd = "test/tmp/port-shell-path-preserved"
  let fake_bin = cwd <> "/fake-bin"
  let tool_bin = cwd <> "/tool-bin"
  let fake_bash = fake_bin <> "/bash"
  let fake_tool = tool_bin <> "/path-sensitive-tool"
  let bash_log = cwd <> "/bash.log"
  let real_bash = real_bash_path()
  test_helpers.reset_dir(cwd)
  let assert Ok(Nil) = simplifile.create_directory_all(fake_bin)
  let assert Ok(Nil) = simplifile.create_directory_all(tool_bin)
  let assert Ok(Nil) =
    simplifile.write(fake_bash, fake_bash_that_resets_path_script(real_bash))
  let assert Ok(Nil) =
    simplifile.write(fake_tool, path_sensitive_tool_script(real_bash))
  chmod_executable(fake_bash)
  chmod_executable(fake_tool)
  let assert Ok(fake_bin_path) = scherzo_path.absolute(fake_bin)
  let assert Ok(tool_bin_path) = scherzo_path.absolute(tool_bin)
  let assert Ok(bash_log_path) = scherzo_path.absolute(bash_log)

  let test_path =
    fake_bin_path
    <> ":"
    <> prepend_path(tool_bin_path, scherzo_path.env("PATH"))

  let start_result =
    port.start_with_env("path-sensitive-tool", cwd, [
      #("PATH", test_path),
      #("SCHERZO_TEST_REAL_BASH", real_bash),
      #("SCHERZO_TEST_FAKE_BASH_LOG", bash_log_path),
    ])

  let assert Ok(process) = start_result
  assert_process_output(process, "tool-ok")
  let assert Ok(log) = simplifile.read(bash_log_path)
  assert fake_bash_call_count(log) >= 2
}

pub fn port_launchers_resolve_bash_from_path_test() {
  let cwd = "test/tmp/port-path-bash"
  let fake_bin = cwd <> "/bin"
  let fake_bash = fake_bin <> "/bash"
  let bash_log = cwd <> "/bash.log"
  test_helpers.reset_dir(cwd)
  let assert Ok(Nil) = simplifile.create_directory_all(fake_bin)
  let assert Ok(Nil) = simplifile.write(fake_bash, fake_bash_script())
  chmod_executable(fake_bash)
  let assert Ok(fake_bin_path) = scherzo_path.absolute(fake_bin)
  let assert Ok(bash_log_path) = scherzo_path.absolute(bash_log)

  let original_path = scherzo_path.env("PATH")
  let original_path_value = optional_env_value(original_path)
  let fake_env = [
    #("PATH", prepend_path(fake_bin_path, original_path)),
    #("SCHERZO_TEST_ORIGINAL_PATH", original_path_value),
    #("SCHERZO_TEST_FAKE_BASH_LOG", bash_log_path),
  ]

  let shell_result =
    port.start_with_env("printf '%s\n' shell-ok", cwd, fake_env)
  let argv_result =
    port.start_argv("bash", ["-c", "printf '%s\n' argv-ok"], cwd, fake_env)
  let argv_input_result =
    port.start_argv_with_input(
      "bash",
      ["-c", "cat"],
      cwd,
      fake_env,
      "input-ok\n",
    )

  let assert Ok(shell_process) = shell_result
  assert_process_output(shell_process, "shell-ok")
  let assert Ok(argv_process) = argv_result
  assert_process_output(argv_process, "argv-ok")
  let assert Ok(argv_input_process) = argv_input_result
  assert_process_output(argv_input_process, "input-ok")

  let assert Ok(log) = simplifile.read(bash_log_path)
  assert fake_bash_call_count(log) >= 4
}

pub fn port_start_argv_with_input_preserves_current_path_for_child_test() {
  let cwd = "test/tmp/port-argv-input-path-preserved"
  let tool_bin = cwd <> "/tool-bin"
  let fake_tool = tool_bin <> "/path-sensitive-tool"
  test_helpers.reset_dir(cwd)
  let assert Ok(Nil) = simplifile.create_directory_all(tool_bin)
  let assert Ok(Nil) =
    simplifile.write(fake_tool, path_sensitive_stdin_tool_script())
  chmod_executable(fake_tool)
  let assert Ok(tool_bin_path) = scherzo_path.absolute(tool_bin)

  let start_result =
    port.start_argv_with_input(
      "path-sensitive-tool",
      [],
      cwd,
      [
        #("PATH", prepend_path(tool_bin_path, scherzo_path.env("PATH"))),
        #("SCHERZO_TEST_ENV", "kept"),
      ],
      "payload\n",
    )

  let assert Ok(process) = start_result
  assert_process_output(process, "tool-ok payload kept")
}

fn fake_bash_script() -> String {
  "#!/bin/sh\n"
  <> "printf 'called\\n' >> \"$SCHERZO_TEST_FAKE_BASH_LOG\"\n"
  <> "PATH=\"$SCHERZO_TEST_ORIGINAL_PATH\"\n"
  <> "export PATH\n"
  <> "exec bash \"$@\"\n"
}

fn fake_bash_that_resets_path_script(real_bash: String) -> String {
  "#!"
  <> real_bash
  <> "\n"
  <> "printf 'called %s\\n' \"$1\" >> \"$SCHERZO_TEST_FAKE_BASH_LOG\"\n"
  <> "PATH=/definitely-not-a-real-scherzo-test-path\n"
  <> "export PATH\n"
  <> "exec \"$SCHERZO_TEST_REAL_BASH\" \"$@\"\n"
}

fn path_sensitive_tool_script(real_bash: String) -> String {
  "#!" <> real_bash <> "\n" <> "printf 'tool-ok\\n'\n"
}

fn path_sensitive_stdin_tool_script() -> String {
  "#!/bin/sh\n"
  <> "IFS= read -r line\n"
  <> "printf 'tool-ok %s %s\\n' \"$line\" \"$SCHERZO_TEST_ENV\"\n"
}

fn real_bash_path() -> String {
  let assert Ok(process) =
    port.start_argv("sh", ["-c", "command -v bash"], ".", [])
  let assert Ok(real_bash) = port.read_stdout_line(process, 1000)
  let assert Ok(0) = port.await_exit(process, 1000)
  real_bash
}

fn chmod_executable(path: String) -> Nil {
  let assert Ok(process) = port.start_argv("chmod", ["+x", path], ".", [])
  let assert Ok(0) = port.await_exit(process, 1000)
  Nil
}

fn prepend_path(path: String, original_path: Option(String)) -> String {
  case original_path {
    Some(original_path) -> path <> ":" <> original_path
    None -> path
  }
}

fn optional_env_value(value: Option(String)) -> String {
  case value {
    Some(value) -> value
    None -> ""
  }
}

fn assert_process_output(process: port.Process, expected: String) -> Nil {
  let assert Ok(stdout) = port.read_stdout_line(process, 1000)
  assert stdout == expected
  let assert Ok(0) = port.await_exit(process, 1000)
  Nil
}

fn fake_bash_call_count(log: String) -> Int {
  let trimmed = string.trim(log)
  case trimmed == "" {
    True -> 0
    False -> string.split(trimmed, on: "\n") |> list.length
  }
}

fn assert_launch_wrapper_records_child_pid(process: port.Process) -> Nil {
  let assert Ok(temp_dir) = port.temp_dir_for_test(process)
  let assert Ok(child_pid) = simplifile.read(temp_dir <> "/child.pid")
  let _ = port.terminate(process)

  assert string.trim(child_pid) != ""
}

pub fn port_start_waits_until_launch_wrapper_records_child_pid_test() {
  let cwd = "test/tmp/port-start-ready"
  test_helpers.reset_dir(cwd)

  let assert Ok(process) = port.start("sleep 60", cwd)
  assert_launch_wrapper_records_child_pid(process)
}

pub fn port_start_argv_waits_until_launch_wrapper_records_child_pid_test() {
  let cwd = "test/tmp/port-start-argv-ready"
  test_helpers.reset_dir(cwd)

  let assert Ok(process) = port.start_argv("sh", ["-c", "sleep 60"], cwd, [])
  assert_launch_wrapper_records_child_pid(process)
}

pub fn port_start_argv_with_input_waits_until_launch_wrapper_records_child_pid_test() {
  let cwd = "test/tmp/port-start-argv-input-ready"
  test_helpers.reset_dir(cwd)

  let assert Ok(process) =
    port.start_argv_with_input("sh", ["-c", "sleep 60"], cwd, [], "")
  assert_launch_wrapper_records_child_pid(process)
}

pub fn port_terminate_drains_exit_status_after_stdout_test() {
  let cwd = "test/tmp/port-terminate-drains-exit"
  test_helpers.reset_dir(cwd)

  let assert Ok(process) = port.start("printf 'ready\\n'", cwd)
  let assert Ok(stdout) = port.read_stdout_line(process, 1000)
  assert stdout == "ready"
  let assert Ok(Nil) = port.terminate(process)

  assert drain_port_exit_messages(process) == 0
}

pub fn port_terminate_exits_child_test() {
  let cwd = "test/tmp/port-terminate"
  test_helpers.reset_dir(cwd)

  let child_pid_file = cwd <> "/child.pid"
  let assert Ok(process) =
    port.start("sleep 60 & echo $! > child.pid; wait", cwd)
  let assert Ok(child_pid) = read_pid_file(child_pid_file)
  assert pid_alive(child_pid)
  let terminate_result = port.terminate(process)
  let await_result = port.await_exit(process, 1000)
  let child_dead = wait_until_dead(child_pid, 50)

  let assert Ok(Nil) = terminate_result
  let assert Ok(_) = await_result
  assert child_dead
}

pub fn port_non_owner_await_exit_stops_cleanup_watcher_test() {
  let cwd = "test/tmp/port-non-owner-cleanup"
  test_helpers.reset_dir(cwd)

  let process_subject = process.new_subject()
  let owner_barrier = test_async.new_barrier()
  let _owner =
    process.spawn_unlinked(fn() {
      let assert Ok(started_process) = port.start("printf 'done\\n'", cwd)
      process.send(process_subject, started_process)
      test_async.block_until_released(owner_barrier)
    })
  let assert Ok(started_process) =
    process.receive(process_subject, within: 1000)
  assert process_cleanup_watcher_alive(started_process)

  let assert Ok(0) = port.await_exit(started_process, 1000)

  assert wait_until_cleanup_watcher_stopped(started_process, 50)
  test_async.release_barrier(owner_barrier)
}

pub fn port_owner_death_terminates_child_test() {
  assert_owner_death_terminates_child("test/tmp/port-owner-death", fn(cwd) {
    port.start(owner_death_command(), cwd)
  })
}

pub fn port_start_argv_owner_death_terminates_child_test() {
  assert_owner_death_terminates_child("test/tmp/port-argv-owner-death", fn(cwd) {
    port.start_argv("sh", ["-c", owner_death_command()], cwd, [])
  })
}

fn assert_owner_death_terminates_child(
  cwd: String,
  start_process: fn(String) -> Result(port.Process, port.PortError),
) -> Nil {
  test_helpers.reset_dir(cwd)

  let child_pid_file = cwd <> "/child.pid"
  let side_effect_file = cwd <> "/side-effect"
  let owner_ready = process.new_subject()
  let owner_barrier = test_async.new_barrier()
  let owner =
    process.spawn_unlinked(fn() {
      let assert Ok(_process) = start_process(cwd)
      process.send(owner_ready, Nil)
      test_async.block_until_released(owner_barrier)
    })
  let assert Ok(Nil) = process.receive(owner_ready, within: 1000)
  let assert Ok(child_pid) = read_pid_file(child_pid_file)
  assert pid_alive(child_pid)

  process.kill(owner)
  test_async.release_barrier_if_waiting(owner_barrier)

  assert wait_until_dead(child_pid, 50)
  assert simplifile.is_file(side_effect_file) == Ok(False)
}

fn owner_death_command() -> String {
  "echo $$ > child.pid; sleep 60; echo leaked > side-effect"
}

pub fn port_await_exit_cleans_residual_descendant_test() {
  let cwd = "test/tmp/port-await-descendant"
  test_helpers.reset_dir(cwd)

  let child_pid_file = cwd <> "/orphan.pid"
  let assert Ok(process) =
    port.start("sleep 60 & echo $! > orphan.pid; exit 0", cwd)
  let assert Ok(child_pid) = read_pid_file(child_pid_file)
  let assert Ok(0) = port.await_exit(process, 1000)
  assert wait_until_dead(child_pid, 50)
}

pub fn port_await_exit_succeeds_after_stdout_when_residual_group_exists_test() {
  let cwd = "test/tmp/port-await-stdout-descendant"
  test_helpers.reset_dir(cwd)

  let child_pid_file = cwd <> "/orphan.pid"
  let assert Ok(process) =
    port.start(
      "sleep 60 & echo $! > orphan.pid; printf 'ready\\n'; exit 0",
      cwd,
    )
  let assert Ok(child_pid) = read_pid_file(child_pid_file)
  let assert Ok(stdout) = port.read_stdout_line(process, 1000)
  assert stdout == "ready"
  let assert Ok(0) = port.await_exit(process, 1000)
  assert wait_until_dead(child_pid, 50)
}

pub fn port_await_exit_discards_queued_stdout_messages_test() {
  let cwd = "test/tmp/port-await-discard-stdout"
  test_helpers.reset_dir(cwd)

  let assert Ok(process) =
    port.start("printf 'hook stdout one\\nhook stdout two\\n'", cwd)
  assert wait_for_port_data_and_requeue(process, 1000)
  let assert Ok(0) = port.await_exit(process, 1000)
  assert drain_port_data_messages(process) == 0
}

pub fn port_await_exit_discards_buffered_stdout_state_test() {
  let cwd = "test/tmp/port-await-discard-buffered-stdout"
  test_helpers.reset_dir(cwd)

  let assert Ok(process) =
    port.start("printf 'hook stdout one\\nhook stdout two\\n'", cwd)
  let assert Ok(stdout) = port.read_stdout_line(process, 1000)
  assert stdout == "hook stdout one"
  let assert Ok(0) = port.await_exit(process, 1000)
  let assert Error(_) = port.read_stdout_line(process, 20)
}

pub fn port_await_exit_times_out_when_residual_cleanup_exceeds_deadline_test() {
  let cwd = "test/tmp/port-await-residual-cleanup-timeout"
  test_helpers.reset_dir(cwd)

  let child_pid_file = cwd <> "/orphan.pid"
  let assert Ok(process) =
    port.start(
      "sh -c 'trap \"\" TERM; while :; do sleep 1; done' & echo $! > orphan.pid; exit 0",
      cwd,
    )
  let assert Ok(temp_dir) = port.temp_dir_for_test(process)
  let assert Ok(child_pid) = read_pid_file(child_pid_file)
  assert wait_until_file(temp_dir <> "/exit.status", 50)
  assert pid_alive(child_pid)
  let assert Error(port.ReadTimeout) = port.await_exit(process, 10)
  let assert Ok(Nil) = port.terminate(process)
  assert wait_until_dead(child_pid, 50)
}

pub fn port_await_exit_preserves_nonzero_status_with_residual_descendant_test() {
  let cwd = "test/tmp/port-await-nonzero-descendant"
  test_helpers.reset_dir(cwd)

  let child_pid_file = cwd <> "/orphan.pid"
  let assert Ok(process) =
    port.start("sleep 60 & echo $! > orphan.pid; exit 7", cwd)
  let assert Ok(child_pid) = read_pid_file(child_pid_file)
  let assert Ok(7) = port.await_exit(process, 1000)
  assert wait_until_dead(child_pid, 50)
}

pub fn port_start_argv_await_exit_preserves_nonzero_status_with_residual_descendant_test() {
  let cwd = "test/tmp/port-await-argv-nonzero-descendant"
  test_helpers.reset_dir(cwd)

  let child_pid_file = cwd <> "/orphan.pid"
  let assert Ok(process) =
    port.start_argv(
      "sh",
      ["-c", "sleep 60 & echo $! > orphan.pid; exit 9"],
      cwd,
      [],
    )
  let assert Ok(child_pid) = read_pid_file(child_pid_file)
  let assert Ok(9) = port.await_exit(process, 1000)
  assert wait_until_dead(child_pid, 50)
}

pub fn port_start_argv_with_input_await_exit_preserves_nonzero_status_with_residual_descendant_test() {
  let cwd = "test/tmp/port-await-argv-input-nonzero-descendant"
  test_helpers.reset_dir(cwd)

  let child_pid_file = cwd <> "/orphan.pid"
  let assert Ok(process) =
    port.start_argv_with_input(
      "sh",
      ["-c", "cat >/dev/null; sleep 60 & echo $! > orphan.pid; exit 11"],
      cwd,
      [],
      "ignored\n",
    )
  let assert Ok(child_pid) = read_pid_file(child_pid_file)
  let assert Ok(11) = port.await_exit(process, 1000)
  assert wait_until_dead(child_pid, 50)
}

pub fn port_max_line_handling_test() {
  let cwd = "test/tmp/port-lines"
  test_helpers.reset_dir(cwd)

  let under = port.max_stdout_line_length - 1
  let assert Ok(process_ok) =
    port.start(
      "node -e 'process.stdout.write(\"a\".repeat("
        <> int_to_string(under)
        <> ") + \"\\n\")'",
      cwd,
    )
  let assert Ok(line) = port.read_stdout_line(process_ok, 5000)
  assert string.length(line) == under

  let over = port.max_stdout_line_length + 1
  let assert Ok(process_over) =
    port.start(
      "node -e 'process.stdout.write(\"a\".repeat("
        <> int_to_string(over)
        <> ") + \"\\n\")'",
      cwd,
    )
  let over_result = port.read_stdout_line(process_over, 5000)
  let _ = port.terminate(process_over)
  let assert Error(port.LineTooLong(_)) = over_result
}

pub fn port_returns_typed_launch_errors_test() {
  let assert Error(port.CwdNotDirectory) =
    port.start("echo should not run", "test/tmp/port-missing-cwd")

  let cwd = "test/tmp/port-invalid-launch"
  test_helpers.reset_dir(cwd)
  let assert Error(port.InvalidCommand(_)) = port.start("   ", cwd)
  let assert Error(port.InvalidExecutable(_)) = port.start_argv("", [], cwd, [])
}

pub fn port_read_timeout_leaves_process_terminable_test() {
  let cwd = "test/tmp/port-read-timeout"
  test_helpers.reset_dir(cwd)

  let assert Ok(process) = port.start("sleep 1", cwd)
  let assert Error(port.ReadTimeout) = port.read_stdout_line(process, 20)
  let assert Ok(Nil) = port.terminate(process)
}

pub fn port_read_timeout_is_absolute_for_partial_lines_test() {
  let cwd = "test/tmp/port-partial-line-timeout"
  test_helpers.reset_dir(cwd)

  let assert Ok(process) =
    port.start(
      "i=0; while [ $i -lt 10 ]; do printf x; i=$((i + 1)); sleep 0.03; done; sleep 1",
      cwd,
    )
  let started_ms = monotonic_ms()
  let result = port.read_stdout_line(process, 120)
  let elapsed_ms = monotonic_ms() - started_ms
  let _ = port.terminate(process)

  let assert Error(port.ReadTimeout) = result
  assert elapsed_ms < 250
}

pub fn port_diagnostics_survive_await_cleanup_test() {
  let cwd = "test/tmp/port-diagnostics-cleanup"
  test_helpers.reset_dir(cwd)

  let assert Ok(process) = port.start("echo diagnostic >&2", cwd)
  let assert Ok(temp_dir) = port.temp_dir_for_test(process)
  let assert Ok(True) = simplifile.is_directory(temp_dir)
  let assert Ok(0) = port.await_exit(process, 1000)
  let assert Ok(False) = simplifile.is_directory(temp_dir)
  let assert Ok(diagnostics) = port.read_diagnostics(process)
  assert string.contains(diagnostics, "diagnostic")
}

pub fn port_terminate_cleans_temp_storage_test() {
  let cwd = "test/tmp/port-terminate-cleanup"
  test_helpers.reset_dir(cwd)

  let assert Ok(process) = port.start("sleep 60", cwd)
  let assert Ok(temp_dir) = port.temp_dir_for_test(process)
  let assert Ok(True) = simplifile.is_directory(temp_dir)
  let assert Ok(Nil) = port.terminate(process)
  let assert Ok(False) = simplifile.is_directory(temp_dir)
}

fn read_pid_file(path: String) -> Result(Int, Nil) {
  read_pid_file_attempts(path, 250)
}

fn read_pid_file_attempts(path: String, attempts: Int) -> Result(Int, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case simplifile.read(path) {
        Ok(contents) ->
          case int.parse(string.trim(contents)) {
            Ok(pid) -> Ok(pid)
            Error(_) -> retry_read_pid_file(path, attempts)
          }
        Error(_) -> retry_read_pid_file(path, attempts)
      }
  }
}

fn retry_read_pid_file(path: String, attempts: Int) -> Result(Int, Nil) {
  process.sleep(20)
  read_pid_file_attempts(path, attempts - 1)
}

fn wait_until_file(path: String, attempts: Int) -> Bool {
  case simplifile.is_file(path) {
    Ok(True) -> True
    _ ->
      case attempts <= 0 {
        True -> False
        False -> {
          process.sleep(5)
          wait_until_file(path, attempts - 1)
        }
      }
  }
}

fn wait_until_dead(pid: Int, attempts: Int) -> Bool {
  case pid_alive(pid) {
    False -> True
    True ->
      case attempts <= 0 {
        True -> False
        False -> {
          process.sleep(20)
          wait_until_dead(pid, attempts - 1)
        }
      }
  }
}

fn wait_until_cleanup_watcher_stopped(
  port_process: port.Process,
  attempts: Int,
) -> Bool {
  case process_cleanup_watcher_alive(port_process) {
    False -> True
    True ->
      case attempts <= 0 {
        True -> False
        False -> {
          process.sleep(20)
          wait_until_cleanup_watcher_stopped(port_process, attempts - 1)
        }
      }
  }
}

@external(erlang, "scherzo_test_ffi", "pid_alive")
fn pid_alive(pid: Int) -> Bool

@external(erlang, "scherzo_test_ffi", "process_cleanup_watcher_alive")
fn process_cleanup_watcher_alive(port_process: port.Process) -> Bool

@external(erlang, "scherzo_test_ffi", "wait_for_port_data_and_requeue")
fn wait_for_port_data_and_requeue(
  port_process: port.Process,
  timeout_ms: Int,
) -> Bool

@external(erlang, "scherzo_test_ffi", "drain_port_data_messages")
fn drain_port_data_messages(port_process: port.Process) -> Int

@external(erlang, "scherzo_test_ffi", "drain_port_exit_messages")
fn drain_port_exit_messages(port_process: port.Process) -> Int

@external(erlang, "scherzo_time_ffi", "monotonic_ms")
fn monotonic_ms() -> Int

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
