import gleeunit/should
import scherzo/agent/driver.{
  type Driver, Command, Driver, Failure, Output, Success,
}
import scherzo/core/task
import scherzo/core/types.{AgentConfig, Claude}

/// Create a simple test driver
fn test_driver() -> Driver {
  Driver(
    name: "test",
    build_command: fn(t, config) {
      Command(
        executable: "echo",
        args: [t.title],
        working_dir: config.working_dir,
        env: [],
      )
    },
    parse_output: fn(line) { Output(line) },
    detect_result: fn(output, exit_code) {
      case exit_code {
        0 -> Success(output)
        _ -> Failure("failed", exit_code)
      }
    },
  )
}

pub fn build_command_uses_driver_function_test() {
  let drv = test_driver()
  let t = task.new("task-1", "Hello World", "Test task")
  let config =
    AgentConfig(
      id: "agent-1",
      provider: Claude,
      working_dir: "/tmp",
      max_retries: 1,
      timeout_ms: 60_000,
      interactive: False,
    )

  let command = driver.build_command(drv, t, config)

  command.executable |> should.equal("echo")
  command.working_dir |> should.equal("/tmp")
}

pub fn parse_output_uses_driver_function_test() {
  let drv = test_driver()

  let result = driver.parse_output(drv, "test line")

  case result {
    Output(text) -> text |> should.equal("test line")
    _ -> should.fail()
  }
}

pub fn detect_result_uses_driver_function_test() {
  let drv = test_driver()

  let success = driver.detect_result(drv, "output", 0)
  let failure = driver.detect_result(drv, "error", 1)

  case success {
    Success(_) -> should.be_true(True)
    _ -> should.fail()
  }

  case failure {
    Failure(_, code) -> code |> should.equal(1)
    _ -> should.fail()
  }
}
