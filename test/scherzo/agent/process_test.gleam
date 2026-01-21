import gleam/option.{None}
import gleeunit/should
import scherzo/agent/driver.{
  type Driver, Command, Driver, Failure, Output, Success,
}
import scherzo/agent/process.{Idle, Running, StartFailed, Started}
import scherzo/core/task.{type Task, Normal, Pending, Task}
import scherzo/core/types.{type AgentConfig, AgentConfig, Claude}

fn make_test_config() -> AgentConfig {
  AgentConfig(
    id: "agent-1",
    provider: Claude,
    working_dir: ".",
    max_retries: 3,
    timeout_ms: 60_000,
  )
}

fn make_mock_driver() -> Driver {
  Driver(
    name: "mock",
    build_command: fn(_task, _config) {
      Command(executable: "echo", args: ["test"], working_dir: ".", env: [])
    },
    parse_output: fn(line) { Output(line) },
    detect_result: fn(output, exit_code) {
      case exit_code {
        0 -> Success(output)
        _ -> Failure("Error", exit_code)
      }
    },
  )
}

fn make_test_task() -> Task {
  Task(
    id: "task-1",
    title: "Test Task",
    description: "A test task",
    status: Pending,
    priority: Normal,
    dependencies: [],
    created_at: 1000,
    updated_at: 1000,
    source_id: None,
    jj_change_id: None,
    parent: None,
  )
}

pub fn start_creates_agent_process_test() {
  let config = make_test_config()
  let driver = make_mock_driver()

  let result = process.start("test-agent", config, driver)

  result |> should.be_ok
  let assert Ok(agent) = result

  // Cleanup
  process.stop(agent)
}

pub fn initial_status_is_idle_test() {
  let config = make_test_config()
  let driver = make_mock_driver()
  let assert Ok(agent) = process.start("test-agent", config, driver)

  let status = process.get_status(agent)

  case status {
    Idle -> should.be_true(True)
    _ -> should.fail()
  }

  // Cleanup
  process.stop(agent)
}

pub fn stop_terminates_agent_test() {
  let config = make_test_config()
  let driver = make_mock_driver()
  let assert Ok(agent) = process.start("test-agent", config, driver)

  // This should not hang or error
  process.stop(agent)
  should.be_true(True)
}

pub fn start_result_types_exist_test() {
  // Just verify the types can be constructed
  let _ = Started(change_id: "abc123")
  let _ = StartFailed(reason: "error")
  should.be_true(True)
}

pub fn agent_process_status_types_exist_test() {
  // Verify status types
  let _ = Idle
  let _ = Running(task_id: "t-1", change_id: "c-1", started_at: 1000)
  let _ = process.Completed(task_id: "t-1", result: Success("done"))
  let _ = process.Failed(reason: "error")
  should.be_true(True)
}

pub fn driver_build_command_test() {
  let drv = make_mock_driver()
  let task = make_test_task()
  let config = make_test_config()

  let cmd = driver.build_command(drv, task, config)

  cmd.executable |> should.equal("echo")
  cmd.args |> should.equal(["test"])
  cmd.working_dir |> should.equal(".")
}

pub fn driver_parse_output_test() {
  let drv = make_mock_driver()

  let result = driver.parse_output(drv, "test output")

  case result {
    Output(text) -> text |> should.equal("test output")
    _ -> should.fail()
  }
}

pub fn driver_detect_result_success_test() {
  let drv = make_mock_driver()

  let result = driver.detect_result(drv, "output", 0)

  case result {
    Success(output) -> output |> should.equal("output")
    _ -> should.fail()
  }
}

pub fn driver_detect_result_failure_test() {
  let drv = make_mock_driver()

  let result = driver.detect_result(drv, "error", 1)

  case result {
    Failure(_, exit_code) -> exit_code |> should.equal(1)
    _ -> should.fail()
  }
}
