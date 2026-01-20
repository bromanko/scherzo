/// Orchestrator - coordinates tasks and agents
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import scherzo/agent/checkpoint
import scherzo/agent/driver.{type AgentResult}
import scherzo/agent/drivers/claude
import scherzo/agent/handoff
import scherzo/agent/workspace.{type Workspace}
import scherzo/core/task.{
  type Task, Completed, Failed, InProgress, new as new_task,
}
import scherzo/core/types.{AgentConfig, Claude, default_timeout_ms}
import scherzo/task/source.{type TaskSource}
import scherzo/vcs/jj
import shellout

/// Maximum length for task titles in jj descriptions
const max_title_length = 200

/// Configuration for the orchestrator
pub type OrchestratorConfig {
  OrchestratorConfig(
    /// Working directory for agents
    working_dir: String,
    /// Maximum retries for failed tasks
    max_retries: Int,
    /// Maximum continuations for context exhaustion (default: 5)
    max_continuations: Int,
  )
}

/// Create default config
pub fn default_config(working_dir: String) -> OrchestratorConfig {
  OrchestratorConfig(
    working_dir: working_dir,
    max_retries: 3,
    max_continuations: 5,
  )
}

/// Result of running a task
pub type RunResult {
  RunSuccess(output: String, change_id: String)
  RunFailed(reason: String)
  /// Task exceeded max continuations without completing
  RunExhausted(continuations: Int, last_output: String, change_id: String)
}

/// Result of a single task in a batch
pub type TaskResult {
  TaskResult(task_id: String, title: String, result: RunResult)
}

/// Result of running tasks from a source
pub type BatchResult {
  BatchResult(completed: List(TaskResult), failed: List(TaskResult), total: Int)
}

/// Run a single task with an agent (synchronous, for Phase 2)
pub fn run_task(
  config: OrchestratorConfig,
  title: String,
  description: String,
) -> RunResult {
  // Create the task
  let task_id = generate_task_id()
  let created_task = new_task(task_id, title, description)

  // Start with continuation count 0
  run_task_with_continuation(config, created_task, 0)
}

/// Internal function that handles task execution with continuation support
fn run_task_with_continuation(
  config: OrchestratorConfig,
  task: Task,
  continuation_count: Int,
) -> RunResult {
  // Check if we've exceeded max continuations
  case handoff.should_fail_task(continuation_count, config.max_continuations) {
    True ->
      RunExhausted(
        continuation_count,
        "Task exceeded maximum continuations ("
          <> int.to_string(config.max_continuations)
          <> ")",
        task.id,
      )

    False -> {
      // Build the task description (with continuation context if needed)
      let effective_task = case continuation_count {
        0 -> task
        _ -> {
          // Load checkpoint and build continuation prompt
          let checkpoint_config = checkpoint.default_config(config.working_dir)
          case
            handoff.load_handoff_context(
              checkpoint_config,
              task,
              continuation_count,
            )
          {
            Error(_) -> task
            // No checkpoint, use original task
            Ok(ctx) -> {
              // Build continuation prompt and create new task with it
              let continuation_prompt = handoff.build_continuation_prompt(ctx)
              new_task(
                task.id,
                task.title <> " (continuation)",
                continuation_prompt,
              )
            }
          }
        }
      }

      // Create isolated workspace for this agent
      let workspace_config = workspace.default_config(config.working_dir)
      case workspace.create(workspace_config, effective_task) {
        Error(err) -> RunFailed("Failed to create workspace: " <> err)

        Ok(ws) -> {
          // Run the agent in the workspace and get result
          let result =
            run_agent_in_workspace(
              config,
              ws,
              effective_task,
              continuation_count,
            )

          // Clean up workspace (changes persist in main repo)
          case workspace.destroy(ws) {
            Ok(_) -> Nil
            Error(err) ->
              io.println(
                "Warning: Failed to clean up workspace for task "
                <> task.id
                <> ": "
                <> err,
              )
          }

          // Handle continuation if needed
          case result {
            RunSuccess(_, _) -> result
            RunFailed(_) -> result
            RunExhausted(_, _, _) -> result
            // This case shouldn't happen from run_agent_in_workspace,
            // but handle it for completeness - context exhausted needs continuation
          }
        }
      }
    }
  }
}

/// Run an agent in an isolated workspace
fn run_agent_in_workspace(
  config: OrchestratorConfig,
  ws: Workspace,
  task: Task,
  continuation_count: Int,
) -> RunResult {
  // Create agent config pointing to workspace directory
  let agent_id = "agent-" <> task.id <> "-" <> int.to_string(continuation_count)
  let agent_config =
    AgentConfig(
      id: agent_id,
      provider: Claude,
      working_dir: ws.path,
      max_retries: config.max_retries,
      timeout_ms: default_timeout_ms,
    )

  // Create the claude driver
  let drv = claude.new()

  // Get the change ID from the workspace (workspace.create already made a change)
  case jj.get_current_change(ws.path) {
    Error(err) -> RunFailed("Failed to get workspace change ID: " <> err)

    Ok(change_id) -> {
      // Update jj change description with task info
      case jj.describe(ws.path, "WIP: " <> sanitize_title(task.title)) {
        Error(err) ->
          io.println("Warning: Failed to set initial description: " <> err)
        Ok(_) -> Nil
      }

      // Build the command
      let command = driver.build_command(drv, task, agent_config)

      // Run the agent synchronously
      let agent_result = run_command(command, drv)

      // Update jj change with result
      case update_change_on_completion(ws.path, task, agent_result) {
        Ok(_) -> Nil
        Error(err) ->
          io.println(
            "Warning: Failed to update jj change for task "
            <> task.id
            <> ": "
            <> err,
          )
      }

      case agent_result {
        driver.Success(output) -> RunSuccess(output, change_id)

        driver.Failure(reason, _) -> RunFailed(reason)

        driver.ContextExhausted(_output) -> {
          // Context exhausted - need to continue with a new agent
          // The Stop hook should have already saved a checkpoint
          // We return a special marker and let the caller handle continuation
          run_task_with_continuation(config, task, continuation_count + 1)
        }

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
  task: Task,
  result: AgentResult,
) -> Result(Nil, String) {
  let safe_title = sanitize_title(task.title)
  let description = case result {
    driver.Success(_output) ->
      safe_title <> "\n\nCompleted successfully.\n\nTask: " <> task.id

    driver.Failure(reason, _) ->
      "FAILED: "
      <> safe_title
      <> "\n\nError: "
      <> reason
      <> "\n\nTask: "
      <> task.id

    driver.ContextExhausted(_) ->
      "WIP: "
      <> safe_title
      <> " (context exhausted, needs continuation)\n\nTask: "
      <> task.id

    driver.Interrupted ->
      "INTERRUPTED: " <> safe_title <> "\n\nTask: " <> task.id
  }

  jj.describe(working_dir, description)
}

/// Sanitize a task title for safe use in jj descriptions
/// - Replaces newlines and carriage returns with spaces
/// - Truncates to max_title_length characters
/// - Trims leading/trailing whitespace
fn sanitize_title(title: String) -> String {
  title
  |> string.replace("\n", " ")
  |> string.replace("\r", " ")
  |> string.replace("\t", " ")
  |> string.trim
  |> string.slice(0, max_title_length)
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

/// Run tasks from a TaskSource until queue is empty or max_tasks reached
pub fn run_from_source(
  config: OrchestratorConfig,
  task_source: TaskSource,
  max_tasks: Int,
) -> Result(BatchResult, String) {
  // Get ready tasks from source
  case source.get_ready_tasks(task_source) {
    Error(err) -> Error("Failed to get ready tasks: " <> err)
    Ok(tasks) -> {
      // Limit to max_tasks if specified
      let tasks_to_run = case max_tasks > 0 {
        True -> list.take(tasks, max_tasks)
        False -> tasks
      }

      // Run tasks sequentially, collecting results
      let results =
        tasks_to_run
        |> list.map(fn(task) { run_source_task(config, task_source, task) })

      // Separate completed and failed
      let completed =
        results
        |> list.filter(fn(r) {
          case r.result {
            RunSuccess(_, _) -> True
            _ -> False
          }
        })

      let failed =
        results
        |> list.filter(fn(r) {
          case r.result {
            RunSuccess(_, _) -> False
            _ -> True
          }
        })

      Ok(BatchResult(
        completed: completed,
        failed: failed,
        total: list.length(tasks_to_run),
      ))
    }
  }
}

/// Run a single task from a source, updating status
fn run_source_task(
  config: OrchestratorConfig,
  task_source: TaskSource,
  task: Task,
) -> TaskResult {
  // Mark task as in progress
  case source.update_status(task_source, task.id, InProgress("", 0)) {
    Ok(_) -> Nil
    Error(err) ->
      io.println(
        "Warning: Failed to mark task " <> task.id <> " as in_progress: " <> err,
      )
  }
  io.println("Starting task: " <> task.title <> " (" <> task.id <> ")")

  // Run the task
  let result = run_existing_task(config, task)

  // Update status based on result
  let status_result = case result {
    RunSuccess(_, _) -> {
      io.println("Completed: " <> task.title)
      source.update_status(task_source, task.id, Completed("", 0))
    }
    RunFailed(reason) -> {
      io.println("Failed: " <> task.title <> " - " <> reason)
      source.update_status(task_source, task.id, Failed("", reason, 0))
    }
    RunExhausted(_, _, _) -> {
      io.println("Exhausted: " <> task.title)
      source.update_status(
        task_source,
        task.id,
        Failed("", "Exceeded max continuations", 0),
      )
    }
  }
  case status_result {
    Ok(_) -> Nil
    Error(err) ->
      io.println(
        "Warning: Failed to update status for task " <> task.id <> ": " <> err,
      )
  }

  TaskResult(task_id: task.id, title: task.title, result: result)
}

/// Run an existing Task object (used by run_from_source)
pub fn run_existing_task(config: OrchestratorConfig, task: Task) -> RunResult {
  run_task_with_continuation(config, task, 0)
}
