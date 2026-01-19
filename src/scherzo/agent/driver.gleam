/// Agent driver interface - abstraction for different CLI agents

import scherzo/core/task.{type Task}
import scherzo/core/types.{type AgentConfig}

/// Command to execute an agent
pub type Command {
  Command(
    /// The executable name (e.g., "claude", "codex")
    executable: String,
    /// Command line arguments
    args: List(String),
    /// Working directory
    working_dir: String,
    /// Environment variables to set
    env: List(#(String, String)),
  )
}

/// Parsed output from agent
pub type ParsedOutput {
  /// Agent produced normal output
  Output(text: String)
  /// Agent reported an error
  Error(message: String)
  /// Agent completed successfully
  Completed(summary: String)
  /// Agent needs to continue (context exhaustion)
  NeedsContinuation(reason: String)
}

/// Result of running an agent
pub type AgentResult {
  /// Agent completed the task successfully
  Success(output: String)
  /// Agent failed with an error
  Failure(reason: String, exit_code: Int)
  /// Agent stopped due to context exhaustion
  ContextExhausted(partial_output: String)
  /// Agent was killed/interrupted
  Interrupted
}

/// Driver interface for CLI agents
/// Each agent type (Claude, Codex, Gemini) implements this interface
pub type Driver {
  Driver(
    /// Name of the driver (e.g., "claude", "codex")
    name: String,
    /// Build the command to execute for a task
    build_command: fn(Task, AgentConfig) -> Command,
    /// Parse a line of output from the agent
    parse_output: fn(String) -> ParsedOutput,
    /// Detect if the agent failed based on output and exit code
    detect_result: fn(String, Int) -> AgentResult,
  )
}

/// Build a command using a driver
pub fn build_command(
  driver: Driver,
  task: Task,
  config: AgentConfig,
) -> Command {
  driver.build_command(task, config)
}

/// Parse output using a driver
pub fn parse_output(driver: Driver, line: String) -> ParsedOutput {
  driver.parse_output(line)
}

/// Detect the result of an agent run
pub fn detect_result(
  driver: Driver,
  output: String,
  exit_code: Int,
) -> AgentResult {
  driver.detect_result(output, exit_code)
}
