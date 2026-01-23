/// Orchestrator coordinator - coordinates tasks, agents, and gates
///
/// This module handles work coordination including:
/// - Task assignment and execution
/// - Agent lifecycle management
/// - Gate triggering on task completion
/// - Retry logic for failed gates
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import scherzo/agent/checkpoint
import scherzo/agent/driver.{type AgentResult}
import scherzo/agent/drivers/claude
import scherzo/agent/handoff
import scherzo/agent/workspace.{type Workspace}
import scherzo/config/agent_config
import scherzo/config/types as config
import scherzo/core/shell
import scherzo/core/task.{
  type Task, Completed, Failed, InProgress, new as new_task,
}
import scherzo/core/types.{AgentConfig, Claude, default_timeout_ms}
import scherzo/gates/executor as gate_executor
import scherzo/gates/feedback
import scherzo/gates/types as gate_types
import scherzo/task/source.{type TaskSource}
import scherzo/vcs/jj

/// Configuration for the coordinator
pub type CoordinatorConfig {
  CoordinatorConfig(
    /// Working directory for agents
    working_dir: String,
    /// Maximum retries for failed tasks
    max_retries: Int,
    /// Maximum continuations for context exhaustion (default: 5)
    max_continuations: Int,
    /// Gate configuration
    gates_config: config.GatesConfig,
    /// Retry configuration for gates
    retry_config: config.RetryConfig,
  )
}

/// Create default config
pub fn default_config(working_dir: String) -> CoordinatorConfig {
  CoordinatorConfig(
    working_dir: working_dir,
    max_retries: 3,
    max_continuations: 5,
    gates_config: config.default_gates_config(),
    retry_config: config.default_retry_config(),
  )
}

/// Create config with gates enabled (Gleam-specific gates)
pub fn config_with_gates(working_dir: String) -> CoordinatorConfig {
  CoordinatorConfig(
    working_dir: working_dir,
    max_retries: 3,
    max_continuations: 5,
    gates_config: gate_executor.gleam_test_config(),
    retry_config: config.default_retry_config(),
  )
}

/// Result of running a task
pub type RunResult {
  RunSuccess(output: String, change_id: String)
  RunFailed(reason: String)
  /// Task exceeded max continuations without completing
  RunExhausted(continuations: Int, last_output: String, change_id: String)
  /// Gates failed after max iterations
  RunGatesFailed(gate_name: String, feedback_summary: String)
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
  config: CoordinatorConfig,
  title: String,
  description: String,
) -> RunResult {
  // Create the task
  let task_id = generate_task_id()
  let created_task = new_task(task_id, title, description)

  // Start with continuation count 0 and gate iteration 1
  run_task_with_continuation(config, created_task, 0, 1, None)
}

/// Internal function that handles task execution with continuation and gate support
fn run_task_with_continuation(
  config: CoordinatorConfig,
  task: Task,
  continuation_count: Int,
  gate_iteration: Int,
  gate_feedback: Option(gate_types.GateFeedback),
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
      let effective_task = case continuation_count, gate_feedback {
        // First run, no feedback
        0, None -> task

        // Retry with gate feedback (same context)
        _, Some(fb) -> {
          let feedback_str = feedback.format_for_agent(fb)
          new_task(
            task.id,
            task.title <> " (iteration " <> int.to_string(gate_iteration) <> ")",
            task.description
              <> "\n\n---\n\n## Gate Feedback\n\n"
              <> feedback_str,
          )
        }

        // Continuation from context exhaustion
        _, None -> {
          let checkpoint_config = checkpoint.default_config(config.working_dir)
          case
            handoff.load_handoff_context(
              checkpoint_config,
              task,
              continuation_count,
            )
          {
            Error(_) -> task
            Ok(ctx) -> {
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

      // Load custom agent config (returns empty config if directory doesn't exist,
      // fails if config exists but is invalid)
      case agent_config.load_task_config(config.working_dir) {
        Error(err) -> RunFailed("Invalid agent config: " <> err)

        Ok(custom_config) -> {
          // Create isolated workspace for this agent
          let workspace_config = workspace.default_config(config.working_dir)
          // Generate agent ID for hook commands (not tracked in .scherzo/agents/ for this code path)
          let agent_id = "agent-" <> effective_task.id <> "-coordinator"
          case
            workspace.create(
              workspace_config,
              effective_task,
              agent_id,
              Some(custom_config),
            )
          {
            Error(err) -> RunFailed("Failed to create workspace: " <> err)

            Ok(ws) -> {
              // Run the agent in the workspace and get result
              let agent_result =
                run_agent_in_workspace(
                  config,
                  ws,
                  effective_task,
                  continuation_count,
                )

              // Handle the result - run gates BEFORE destroying workspace
              // so gates can test the agent's changes
              let final_result = case agent_result {
                RunSuccess(output, change_id) -> {
                  // Agent completed - run gates in the workspace where changes were made
                  run_gates_and_handle_result(
                    config,
                    task,
                    ws.path,
                    continuation_count,
                    gate_iteration,
                    output,
                    change_id,
                  )
                }
                RunFailed(_) -> agent_result
                RunExhausted(_, _, _) -> agent_result
                RunGatesFailed(_, _) -> agent_result
              }

              // Clean up workspace after gates have run (changes persist in main repo)
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

              final_result
            }
          }
        }
      }
    }
  }
}

/// Run gates and handle the result
fn run_gates_and_handle_result(
  config: CoordinatorConfig,
  task: Task,
  workspace_path: String,
  continuation_count: Int,
  gate_iteration: Int,
  output: String,
  change_id: String,
) -> RunResult {
  // Skip gates if none configured
  case config.gates_config.gates {
    [] -> RunSuccess(output, change_id)

    _ -> {
      io.println(
        "Running completion gates for task "
        <> task.id
        <> " (iteration "
        <> int.to_string(gate_iteration)
        <> ")",
      )

      // Execute gates in the workspace where changes were made
      case gate_executor.execute(config.gates_config, workspace_path) {
        Error(err) -> {
          let msg = case err {
            gate_executor.NoGatesConfigured -> "No gates configured"
            gate_executor.ConfigError(m) -> "Config error: " <> m
            gate_executor.ExecutionError(name, m) ->
              "Execution error in " <> name <> ": " <> m
          }
          RunFailed("Gate execution error: " <> msg)
        }

        Ok(gate_result) -> {
          case gate_result {
            gate_types.AllPassed(_) -> {
              io.println("All gates passed for task " <> task.id)
              RunSuccess(output, change_id)
            }

            gate_types.Failed(gate_name, fb, _) -> {
              io.println(
                "Gate '"
                <> gate_name
                <> "' failed for task "
                <> task.id
                <> " - "
                <> fb.summary,
              )

              // Check if we should retry
              case gate_iteration < config.retry_config.max_iterations {
                True -> {
                  io.println(
                    "Retrying task "
                    <> task.id
                    <> " (iteration "
                    <> int.to_string(gate_iteration + 1)
                    <> ")",
                  )
                  // Retry with feedback
                  run_task_with_continuation(
                    config,
                    task,
                    continuation_count,
                    gate_iteration + 1,
                    Some(fb),
                  )
                }

                False -> {
                  io.println(
                    "Task "
                    <> task.id
                    <> " failed after "
                    <> int.to_string(gate_iteration)
                    <> " gate iterations",
                  )
                  RunGatesFailed(gate_name, feedback.format_summary(fb))
                }
              }
            }
          }
        }
      }
    }
  }
}

/// Run an agent in an isolated workspace
fn run_agent_in_workspace(
  config: CoordinatorConfig,
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
      // Coordinator runs agents in background mode (non-interactive)
      interactive: False,
    )

  // Create the claude driver
  let drv = claude.new()

  // Get the change ID from the workspace
  case jj.get_current_change(ws.path) {
    Error(err) -> RunFailed("Failed to get workspace change ID: " <> err)

    Ok(change_id) -> {
      // Update jj change description with task info
      case jj.describe(ws.path, "WIP: " <> jj.sanitize_title(task.title)) {
        Error(err) ->
          io.println("Warning: Failed to set initial description: " <> err)
        Ok(_) -> Nil
      }

      // Build the command
      let command = driver.build_command(drv, task, agent_config)

      // Run the agent synchronously (with 30 minute timeout)
      let agent_result = run_command(command, drv, agent_config.timeout_ms)

      // Update jj change with result
      case jj.describe_task_completion(ws.path, task, agent_result) {
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
          run_task_with_continuation(
            config,
            task,
            continuation_count + 1,
            1,
            // Reset gate iteration on new continuation
            None,
          )
        }

        driver.Interrupted -> RunFailed("Agent was interrupted")
      }
    }
  }
}

/// Run a command and get the result (with timeout)
fn run_command(
  command: driver.Command,
  drv: driver.Driver,
  timeout_ms: Int,
) -> AgentResult {
  case
    shell.run_with_timeout(
      command.executable,
      command.args,
      command.working_dir,
      timeout_ms,
    )
  {
    shell.Success(output) -> driver.detect_result(drv, output, 0)
    shell.Failed(exit_code, output) ->
      driver.detect_result(drv, output, exit_code)
    shell.TimedOut -> driver.Interrupted
  }
}

/// Generate a simple task ID
fn generate_task_id() -> String {
  let timestamp = erlang_system_time()
  "task-" <> int.to_string(timestamp)
}

@external(erlang, "erlang", "system_time")
fn erlang_system_time() -> Int

/// Run tasks from a TaskSource until queue is empty or max_tasks reached
pub fn run_from_source(
  config: CoordinatorConfig,
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
  config: CoordinatorConfig,
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
    RunGatesFailed(gate_name, summary) -> {
      io.println("Gates failed: " <> task.title <> " - " <> gate_name)
      source.update_status(
        task_source,
        task.id,
        Failed("", "Gate failed: " <> gate_name <> " - " <> summary, 0),
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
pub fn run_existing_task(config: CoordinatorConfig, task: Task) -> RunResult {
  run_task_with_continuation(config, task, 0, 1, None)
}
