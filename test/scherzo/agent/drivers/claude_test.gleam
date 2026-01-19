import gleam/list
import gleeunit/should
import scherzo/agent/driver.{ContextExhausted, Failure, Interrupted, Success}
import scherzo/agent/drivers/claude
import scherzo/core/task
import scherzo/core/types.{AgentConfig, Claude}

pub fn new_creates_claude_driver_test() {
  let driver = claude.new()

  driver.name |> should.equal("claude")
}

pub fn build_command_creates_correct_command_test() {
  let driver = claude.new()
  let t = task.new("task-1", "Fix the bug", "Fix the login bug in auth.gleam")
  let config =
    AgentConfig(
      id: "agent-1",
      provider: Claude,
      working_dir: "/tmp/project",
      max_retries: 3,
    )

  let command = driver.build_command(t, config)

  command.executable |> should.equal("claude")
  command.working_dir |> should.equal("/tmp/project")
  // Should include --print and --dangerously-skip-permissions
  list.contains(command.args, "--print") |> should.be_true
  list.contains(command.args, "--dangerously-skip-permissions")
  |> should.be_true
}

pub fn detect_result_returns_success_for_exit_0_test() {
  let driver = claude.new()

  let result = driver.detect_result("Task completed successfully", 0)

  case result {
    Success(output) -> output |> should.equal("Task completed successfully")
    _ -> should.fail()
  }
}

pub fn detect_result_returns_interrupted_for_sigint_test() {
  let driver = claude.new()

  // Exit code 130 = SIGINT (128 + 2)
  let result = driver.detect_result("", 130)

  case result {
    Interrupted -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn detect_result_returns_interrupted_for_sigterm_test() {
  let driver = claude.new()

  // Exit code 143 = SIGTERM (128 + 15)
  let result = driver.detect_result("", 143)

  case result {
    Interrupted -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn detect_result_returns_failure_for_unknown_exit_code_test() {
  let driver = claude.new()

  let result = driver.detect_result("Some error", 1)

  case result {
    Failure(_, exit_code) -> exit_code |> should.equal(1)
    _ -> should.fail()
  }
}

pub fn detect_result_returns_context_exhausted_when_context_in_output_test() {
  let driver = claude.new()

  let result = driver.detect_result("Error: context limit reached", 1)

  case result {
    ContextExhausted(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn executable_returns_claude_test() {
  claude.executable() |> should.equal("claude")
}
