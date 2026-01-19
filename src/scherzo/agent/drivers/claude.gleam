/// Claude Code CLI driver

import gleam/int
import gleam/string
import scherzo/agent/driver.{
  type AgentResult, type Command, type Driver, type ParsedOutput, Command,
  ContextExhausted, Driver, Error, Failure, Interrupted, Output, Success,
}
import scherzo/core/task.{type Task}
import scherzo/core/types.{type AgentConfig}

/// Create a Claude driver instance
pub fn new() -> Driver {
  Driver(
    name: "claude",
    build_command: build_claude_command,
    parse_output: parse_claude_output,
    detect_result: detect_claude_result,
  )
}

/// Build the claude CLI command for a task
fn build_claude_command(task: Task, config: AgentConfig) -> Command {
  let prompt = build_prompt(task)

  let args = [
    // Print mode - non-interactive, runs the prompt and exits
    "--print",
    prompt,
    // Auto-approve all tool use for autonomous operation
    "--dangerously-skip-permissions",
    // Output format for easier parsing
    "--output-format",
    "text",
    // Verbose output to see progress
    "--verbose",
  ]

  Command(
    executable: "claude",
    args: args,
    working_dir: config.working_dir,
    env: [],
  )
}

/// Build the prompt for Claude from a task
fn build_prompt(task: Task) -> String {
  string.concat([
    "Task: ",
    task.title,
    "\n\n",
    task.description,
    "\n\n",
    "Please complete this task. When done, summarize what you accomplished.",
  ])
}

/// Parse a line of output from Claude
fn parse_claude_output(line: String) -> ParsedOutput {
  let trimmed = string.trim(line)

  // Check for error indicators
  case string.starts_with(trimmed, "Error:") {
    True -> Error(trimmed)
    False ->
      case string.contains(trimmed, "context limit") {
        True -> driver.NeedsContinuation("Context limit reached")
        False -> Output(trimmed)
      }
  }
}

/// Detect the result based on full output and exit code
fn detect_claude_result(output: String, exit_code: Int) -> AgentResult {
  case exit_code {
    // Success
    0 -> Success(output)

    // Common interrupt signals
    130 -> Interrupted
    137 -> Interrupted
    143 -> Interrupted

    // Check for context exhaustion in output
    _ ->
      case string.contains(output, "context") {
        True -> ContextExhausted(output)
        False -> Failure("Claude exited with code " <> int.to_string(exit_code), exit_code)
      }
  }
}

/// Get the executable name for Claude
pub fn executable() -> String {
  "claude"
}
