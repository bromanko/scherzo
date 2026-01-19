/// Orchestrator - coordinates tasks and agents

import scherzo/agent/driver.{type AgentResult}
import scherzo/agent/drivers/claude
import scherzo/core/task
import scherzo/core/types.{AgentConfig, Claude}
import scherzo/vcs/jj
import shellout

/// Configuration for the orchestrator
pub type OrchestratorConfig {
  OrchestratorConfig(
    /// Working directory for agents
    working_dir: String,
    /// Maximum retries for failed tasks
    max_retries: Int,
  )
}

/// Result of running a task
pub type RunResult {
  RunSuccess(output: String, change_id: String)
  RunFailed(reason: String)
}

/// Run a single task with an agent (synchronous, for Phase 2)
pub fn run_task(
  config: OrchestratorConfig,
  title: String,
  description: String,
) -> RunResult {
  // Create the task
  let task_id = generate_task_id()
  let t = task.new(task_id, title, description)

  // Create agent config
  let agent_config =
    AgentConfig(
      id: "agent-" <> task_id,
      provider: Claude,
      working_dir: config.working_dir,
      max_retries: config.max_retries,
    )

  // Create the claude driver
  let drv = claude.new()

  // Create a new jj change for this task
  case jj.new_change(config.working_dir, "WIP: " <> title) {
    Error(err) -> RunFailed("Failed to create jj change: " <> err)

    Ok(change_id) -> {
      // Build the command
      let command = driver.build_command(drv, t, agent_config)

      // Run the agent synchronously
      let agent_result = run_command(command, drv)

      // Update jj change with result
      let _ = update_change_on_completion(config.working_dir, t, agent_result)

      case agent_result {
        driver.Success(output) -> RunSuccess(output, change_id)
        driver.Failure(reason, _) -> RunFailed(reason)
        driver.ContextExhausted(output) ->
          RunSuccess(output <> "\n(context exhausted)", change_id)
        driver.Interrupted -> RunFailed("Agent was interrupted")
      }
    }
  }
}

/// Run a command and get the result
fn run_command(command: driver.Command, drv: driver.Driver) -> AgentResult {
  case
    shellout.command(
      run: command.executable,
      with: command.args,
      in: command.working_dir,
      opt: [],
    )
  {
    Ok(output) -> driver.detect_result(drv, output, 0)
    Error(#(exit_code, output)) -> driver.detect_result(drv, output, exit_code)
  }
}

/// Update the jj change description after completion
fn update_change_on_completion(
  working_dir: String,
  t: task.Task,
  result: AgentResult,
) -> Result(Nil, String) {
  let description = case result {
    driver.Success(_output) ->
      t.title <> "\n\nCompleted successfully.\n\nTask: " <> t.id

    driver.Failure(reason, _) ->
      "FAILED: " <> t.title <> "\n\nError: " <> reason <> "\n\nTask: " <> t.id

    driver.ContextExhausted(_) ->
      "WIP: "
      <> t.title
      <> " (context exhausted, needs continuation)\n\nTask: "
      <> t.id

    driver.Interrupted -> "INTERRUPTED: " <> t.title <> "\n\nTask: " <> t.id
  }

  jj.describe(working_dir, description)
}

/// Generate a simple task ID
fn generate_task_id() -> String {
  let timestamp = erlang_system_time()
  "task-" <> int_to_string(timestamp)
}

@external(erlang, "erlang", "system_time")
fn erlang_system_time() -> Int

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(n: Int) -> String
