import gleam/option.{None, Some}
import scherzo/artifact_repository/command_runner

pub fn production_runner_preserves_stdout_after_idle_timeout_test() {
  let runner =
    command_runner.production_with_env(fn(name) {
      case name {
        "SCHERZO_COMMAND_RUNNER_STDOUT_IDLE_TIMEOUTS" -> Some("1")
        _ -> None
      }
    })
  let command_runner.Runner(run: run) = runner
  let result =
    run(command_runner.sh("bash", ["-c", "sleep 0.1; printf delayed"], "."))

  let assert Ok(output) = result
  assert output.exit_code == 0
  assert output.stdout == "delayed"
}
