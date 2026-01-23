import gleam/list
import gleam/string
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
  // Use interactive: False to test background (--print) mode
  let config =
    AgentConfig(
      id: "agent-1",
      provider: Claude,
      working_dir: "/tmp/project",
      max_retries: 3,
      timeout_ms: 60_000,
      interactive: False,
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

// ---------------------------------------------------------------------------
// Interactive Mode Tests
// ---------------------------------------------------------------------------

pub fn build_command_interactive_mode_omits_print_flag_test() {
  let driver = claude.new()
  let t = task.new("task-1", "Fix the bug", "Fix the login bug")
  // Interactive mode should NOT include --print
  let config =
    AgentConfig(
      id: "agent-1",
      provider: Claude,
      working_dir: "/tmp/project",
      max_retries: 3,
      timeout_ms: 60_000,
      interactive: True,
    )

  let command = driver.build_command(t, config)

  command.executable |> should.equal("claude")
  // Should NOT include --print in interactive mode
  list.contains(command.args, "--print") |> should.be_false
  // Should still include --dangerously-skip-permissions
  list.contains(command.args, "--dangerously-skip-permissions")
  |> should.be_true
}

pub fn build_command_interactive_mode_includes_prompt_test() {
  let driver = claude.new()
  let t = task.new("task-1", "Fix the bug", "Fix the login bug")
  let config =
    AgentConfig(
      id: "agent-1",
      provider: Claude,
      working_dir: "/tmp/project",
      max_retries: 3,
      timeout_ms: 60_000,
      interactive: True,
    )

  let command = driver.build_command(t, config)

  // The prompt should be in the args (task title and description)
  let args_str = list.fold(command.args, "", fn(acc, arg) { acc <> " " <> arg })
  args_str |> string.contains("Fix the bug") |> should.be_true
}

pub fn build_command_background_mode_includes_print_flag_test() {
  let driver = claude.new()
  let t = task.new("task-1", "Fix the bug", "Fix the login bug")
  // Background mode (interactive: False) should include --print
  let config =
    AgentConfig(
      id: "agent-1",
      provider: Claude,
      working_dir: "/tmp/project",
      max_retries: 3,
      timeout_ms: 60_000,
      interactive: False,
    )

  let command = driver.build_command(t, config)

  // Should include --print in background mode
  list.contains(command.args, "--print") |> should.be_true
  // Should include --verbose in background mode
  list.contains(command.args, "--verbose") |> should.be_true
}

pub fn build_command_background_mode_includes_output_format_test() {
  let driver = claude.new()
  let t = task.new("task-1", "Fix the bug", "Fix the login bug")
  let config =
    AgentConfig(
      id: "agent-1",
      provider: Claude,
      working_dir: "/tmp/project",
      max_retries: 3,
      timeout_ms: 60_000,
      interactive: False,
    )

  let command = driver.build_command(t, config)

  // Background mode should include --output-format text
  list.contains(command.args, "--output-format") |> should.be_true
  list.contains(command.args, "text") |> should.be_true
}

pub fn build_command_sets_task_id_env_var_test() {
  let driver = claude.new()
  let t = task.new("my-task-123", "Fix the bug", "Fix the login bug")
  let config =
    AgentConfig(
      id: "agent-1",
      provider: Claude,
      working_dir: "/tmp/project",
      max_retries: 3,
      timeout_ms: 60_000,
      interactive: False,
    )

  let command = driver.build_command(t, config)

  // Should set SCHERZO_TASK_ID environment variable
  list.contains(command.env, #("SCHERZO_TASK_ID", "my-task-123"))
  |> should.be_true
}

pub fn build_command_sets_agent_id_env_var_test() {
  let driver = claude.new()
  let t = task.new("my-task-123", "Fix the bug", "Fix the login bug")
  let config =
    AgentConfig(
      id: "agent-my-task-123-12345",
      provider: Claude,
      working_dir: "/tmp/project",
      max_retries: 3,
      timeout_ms: 60_000,
      interactive: False,
    )

  let command = driver.build_command(t, config)

  // Should set SCHERZO_AGENT_ID environment variable
  list.contains(command.env, #("SCHERZO_AGENT_ID", "agent-my-task-123-12345"))
  |> should.be_true
}
