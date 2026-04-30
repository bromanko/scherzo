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
  let assert Ok(pwd) = port.read_stdout_line(process, 1000)
  assert string.ends_with(pwd, "/" <> cwd)
  let assert Ok(Nil) = port.send_line(process, "hello")
  let assert Ok(line) = port.read_stdout_line(process, 1000)
  assert line == "hello"
  let assert Ok(Nil) = port.terminate(process)
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

  let assert Ok(process) = port.start("while true; do sleep 60; done", cwd)
  let assert Ok(Nil) = port.terminate(process)
  let assert Ok(_) = port.await_exit(process, 1000)
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
  let assert Error(port.LineTooLong) = port.read_stdout_line(process_over, 5000)
  let _ = port.terminate(process_over)
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(value: Int) -> String
