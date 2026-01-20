/// Orchestrator - coordinates tasks and agents
import gleam/int
import gleam/io
import gleam/list
import scherzo/agent/checkpoint
import scherzo/agent/driver.{type AgentResult}
import scherzo/agent/drivers/claude
import scherzo/agent/handoff
import scherzo/core/task.{type Task, Completed, Failed, InProgress}
import scherzo/core/types.{AgentConfig, Claude}
import scherzo/task/source.{type TaskSource}
import scherzo/vcs/jj
import shellout

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
  let t = task.new(task_id, title, description)

  // Start with continuation count 0
  run_task_with_continuation(config, t, 0)
}

/// Internal function that handles task execution with continuation support
fn run_task_with_continuation(
  config: OrchestratorConfig,
  t: Task,
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
        t.id,
      )

    False -> {
      // Create agent config
      let agent_id =
        "agent-" <> t.id <> "-" <> int.to_string(continuation_count)
      let agent_config =
        AgentConfig(
          id: agent_id,
          provider: Claude,
          working_dir: config.working_dir,
          max_retries: config.max_retries,
        )

      // Create the claude driver
      let drv = claude.new()

      // Build the task description (with continuation context if needed)
      let effective_task = case continuation_count {
        0 -> t
        _ -> {
          // Load checkpoint and build continuation prompt
          let checkpoint_config = checkpoint.default_config(config.working_dir)
          case
            handoff.load_handoff_context(
              checkpoint_config,
              t,
              continuation_count,
            )
          {
            Error(_) -> t
            // No checkpoint, use original task
            Ok(ctx) -> {
              // Build continuation prompt and create new task with it
              let continuation_prompt = handoff.build_continuation_prompt(ctx)
              task.new(t.id, t.title <> " (continuation)", continuation_prompt)
            }
          }
        }
      }

      // Create a new jj change for this task (only on first run)
      let change_result = case continuation_count {
        0 -> jj.new_change(config.working_dir, "WIP: " <> t.title)
        _ -> jj.get_current_change(config.working_dir)
      }

      case change_result {
        Error(err) -> RunFailed("Failed to create/get jj change: " <> err)

        Ok(change_id) -> {
          // Build the command
          let command = driver.build_command(drv, effective_task, agent_config)

          // Run the agent synchronously
          let agent_result = run_command(command, drv)

          // Update jj change with result
          case update_change_on_completion(config.working_dir, t, agent_result) {
            Ok(_) -> Nil
            Error(err) ->
              io.println(
                "Warning: Failed to update jj change for task " <> t.id <> ": " <> err,
              )
          }

          case agent_result {
            driver.Success(output) -> RunSuccess(output, change_id)

            driver.Failure(reason, _) -> RunFailed(reason)

            driver.ContextExhausted(_output) -> {
              // Context exhausted - attempt handoff to a new agent
              // The Stop hook should have already saved a checkpoint
              run_task_with_continuation(config, t, continuation_count + 1)
            }

            driver.Interrupted -> RunFailed("Agent was interrupted")
          }
        }
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
        |> list.map(fn(t) { run_source_task(config, task_source, t) })

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
  t: Task,
) -> TaskResult {
  // Mark task as in progress
  case source.update_status(task_source, t.id, InProgress("", 0)) {
    Ok(_) -> Nil
    Error(err) ->
      io.println(
        "Warning: Failed to mark task " <> t.id <> " as in_progress: " <> err,
      )
  }
  io.println("Starting task: " <> t.title <> " (" <> t.id <> ")")

  // Run the task
  let result = run_existing_task(config, t)

  // Update status based on result
  let status_result = case result {
    RunSuccess(_, _) -> {
      io.println("Completed: " <> t.title)
      source.update_status(task_source, t.id, Completed("", 0))
    }
    RunFailed(reason) -> {
      io.println("Failed: " <> t.title <> " - " <> reason)
      source.update_status(task_source, t.id, Failed("", reason, 0))
    }
    RunExhausted(_, _, _) -> {
      io.println("Exhausted: " <> t.title)
      source.update_status(
        task_source,
        t.id,
        Failed("", "Exceeded max continuations", 0),
      )
    }
  }
  case status_result {
    Ok(_) -> Nil
    Error(err) ->
      io.println(
        "Warning: Failed to update status for task " <> t.id <> ": " <> err,
      )
  }

  TaskResult(task_id: t.id, title: t.title, result: result)
}

/// Run an existing Task object (used by run_from_source)
pub fn run_existing_task(config: OrchestratorConfig, t: Task) -> RunResult {
  run_task_with_continuation(config, t, 0)
}
