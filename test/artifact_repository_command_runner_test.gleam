import gleam/option.{type Option, None, Some}
import scherzo/artifact_repository/command_runner
import scherzo/path as scherzo_path

pub fn production_runner_preserves_stdout_after_idle_timeout_test() {
  let result =
    with_env("SCHERZO_COMMAND_RUNNER_STDOUT_IDLE_TIMEOUTS", "1", fn() {
      let runner = command_runner.production()
      let command_runner.Runner(run: run) = runner
      run(command_runner.sh("bash", ["-c", "sleep 0.1; printf delayed"], "."))
    })

  let assert Ok(output) = result
  assert output.exit_code == 0
  assert output.stdout == "delayed"
}

fn with_env(key: String, value: String, run: fn() -> a) -> a {
  let previous = scherzo_path.env(key)
  let assert Ok(Nil) = scherzo_path.set_env(key, value)
  let result = run()
  restore_env(key, previous)
  result
}

fn restore_env(key: String, previous: Option(String)) -> Nil {
  case previous {
    Some(value) -> {
      let assert Ok(Nil) = scherzo_path.set_env(key, value)
      Nil
    }
    None -> {
      let assert Ok(Nil) = scherzo_path.unset_env(key)
      Nil
    }
  }
}
