import gleam/erlang/process
import gleam/int
import gleam/string
import scherzo/port
import simplifile

fn reset_dir(path: String) -> Nil {
  let _ = simplifile.delete(path)
  let assert Ok(Nil) = simplifile.create_directory_all(path)
  Nil
}

pub fn port_cwd_and_stdin_stdout_test() {
  let cwd = "test/tmp/port-cwd"
  reset_dir(cwd)

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
  reset_dir(cwd)

  let assert Ok(process) =
    port.start("echo '{\"ok\":true}'; echo diagnostic >&2", cwd)
  let assert Ok(stdout) = port.read_stdout_line(process, 1000)
  assert stdout == "{\"ok\":true}"
  let assert Error(port.ProcessExited(0)) = port.read_stdout_line(process, 1000)
  let assert Ok(diagnostics) = port.read_diagnostics(process)
  assert string.contains(diagnostics, "diagnostic")
  assert !string.contains(stdout, "diagnostic")
}

pub fn port_start_with_env_applies_environment_test() {
  let cwd = "test/tmp/port-env"
  reset_dir(cwd)

  let assert Ok(process) =
    port.start_with_env("printf '%s\n' \"$SCHERZO_TEST_ENV\"", cwd, [
      #("SCHERZO_TEST_ENV", "hello from env"),
    ])
  let assert Ok(stdout) = port.read_stdout_line(process, 1000)
  assert stdout == "hello from env"
  let assert Error(port.ProcessExited(0)) = port.read_stdout_line(process, 1000)
}

pub fn port_terminate_exits_child_test() {
  let cwd = "test/tmp/port-terminate"
  reset_dir(cwd)

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

pub fn port_await_exit_times_out_while_descendant_survives_test() {
  let cwd = "test/tmp/port-await-descendant"
  reset_dir(cwd)

  let child_pid_file = cwd <> "/orphan.pid"
  let assert Ok(process) =
    port.start("sleep 60 & echo $! > orphan.pid; exit 0", cwd)
  let assert Ok(child_pid) = read_pid_file(child_pid_file)
  assert pid_alive(child_pid)
  let await_result = port.await_exit(process, 100)
  let survived_before_terminate = pid_alive(child_pid)
  let terminate_result = port.terminate(process)
  let child_dead = wait_until_dead(child_pid, 50)

  let assert Error(port.AwaitTimeout) = await_result
  assert survived_before_terminate
  let assert Ok(Nil) = terminate_result
  assert child_dead
}

pub fn port_max_line_handling_test() {
  let cwd = "test/tmp/port-lines"
  reset_dir(cwd)

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
  let assert Error(port.LineTooLong) = over_result
}

fn read_pid_file(path: String) -> Result(Int, Nil) {
  read_pid_file_attempts(path, 50)
}

fn read_pid_file_attempts(path: String, attempts: Int) -> Result(Int, Nil) {
  case attempts <= 0 {
    True -> Error(Nil)
    False ->
      case simplifile.read(path) {
        Ok(contents) -> int.parse(string.trim(contents)) |> result_nil_error
        Error(_) -> {
          process.sleep(20)
          read_pid_file_attempts(path, attempts - 1)
        }
      }
  }
}

fn result_nil_error(result: Result(Int, a)) -> Result(Int, Nil) {
  case result {
    Ok(value) -> Ok(value)
    Error(_) -> Error(Nil)
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

@external(erlang, "scherzo_test_ffi", "pid_alive")
fn pid_alive(pid: Int) -> Bool

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
