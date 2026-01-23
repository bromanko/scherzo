/// Background agent spawning
///
/// Spawns agents in background processes with output routed to named pipes.
/// Agents are registered in the state store and can be monitored via the
/// `agents` command.
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/agent/driver
import scherzo/agent/drivers/claude
import scherzo/agent/workspace.{type Workspace}
import scherzo/config/agent_config
import scherzo/core/task.{type Task}
import scherzo/core/types.{AgentConfig, Claude, default_timeout_ms}
import scherzo/state/agents
import scherzo/ui/pipes
import scherzo/ui/session_manager.{type SessionManager}
import scherzo/vcs/jj

/// Result of spawning an agent
pub type SpawnResult {
  SpawnSuccess(agent_id: String, pipe_path: String)
  SpawnFailed(reason: String)
}

/// Spawn a background agent to work on a task
///
/// This function:
/// 1. Generates a unique agent ID
/// 2. Creates a named pipe for output
/// 3. Registers the agent as Running in the agents directory
/// 4. Spawns an Erlang process to run the agent
/// 5. Optionally creates a tmux pane to display the output
pub fn spawn_agent(
  working_dir: String,
  task: Task,
  session_manager: Option(SessionManager),
) -> Result(#(SpawnResult, Option(SessionManager)), String) {
  let agents_dir = working_dir <> "/" <> agents.default_agents_dir
  // Generate agent ID
  let agent_id = generate_agent_id(task.id)

  // Create pipe config and named pipe
  let pipe_config = pipes.default_config(working_dir)
  case pipes.create_pipe(pipe_config, agent_id) {
    Error(err) -> {
      let msg = case err {
        pipes.DirectoryError(e) -> "Directory error: " <> e
        pipes.CreateError(e) -> "Create error: " <> e
        pipes.PipeNotFound(_) -> "Pipe not found"
        pipes.RemoveError(e) -> "Remove error: " <> e
        pipes.WriteError(e) -> "Write error: " <> e
      }
      Error("Failed to create pipe: " <> msg)
    }

    Ok(pipe_path) -> {
      // Get current timestamp for started_at
      let started_at = erlang_system_time_ms()

      // Create agent config
      let agent_config = make_agent_config(agent_id, working_dir)

      // Register agent as Running in the agents directory
      let agent_state =
        agents.AgentState(
          config: agent_config,
          status: types.Running(task.id, started_at),
          task_id: task.id,
          workspace_path: "",
          pipe_path: pipe_path,
        )
      case agents.save_agent(agents_dir, agent_state) {
        Error(err) ->
          io.println("Warning: Failed to save initial agent state: " <> err)
        Ok(_) -> Nil
      }

      // Create tmux pane if session manager available
      let updated_manager = case session_manager {
        None -> None
        Some(manager) -> {
          case session_manager.add_agent(manager, agent_id) {
            Error(_) -> {
              io.println(
                "Warning: Failed to create tmux pane for agent " <> agent_id,
              )
              Some(manager)
            }
            Ok(#(new_manager, _)) -> Some(new_manager)
          }
        }
      }

      // Spawn background process to run the agent
      // Uses safe_spawn to catch crashes and mark agent as failed
      safe_spawn(
        fn() {
          run_agent_background(
            working_dir,
            task,
            agent_id,
            pipe_path,
            agents_dir,
          )
        },
        fn(error_msg) {
          write_to_pipe(pipe_path, "=== Process Crashed ===")
          write_to_pipe(pipe_path, error_msg)
          mark_agent_failed(agents_dir, agent_id, "Process crashed")
        },
      )

      Ok(#(SpawnSuccess(agent_id, pipe_path), updated_manager))
    }
  }
}

/// Run agent in background process with output to pipe
fn run_agent_background(
  working_dir: String,
  task: Task,
  agent_id: String,
  pipe_path: String,
  agents_dir: String,
) -> Nil {
  let workspace_config = workspace.default_config(working_dir)

  // Chain the setup steps, collecting error context
  let setup_result = {
    use custom_config <- result.try(
      agent_config.load_task_config(working_dir)
      |> result.map_error(fn(e) { "Config error: " <> e }),
    )
    use ws <- result.try(
      workspace.create(workspace_config, task, Some(custom_config))
      |> result.map_error(fn(e) { "Workspace error: " <> e }),
    )
    Ok(ws)
  }

  case setup_result {
    Error(err) -> {
      write_to_pipe(pipe_path, "Error: " <> err)
      mark_agent_failed(agents_dir, agent_id, err)
    }
    Ok(ws) -> {
      // Update agent state with workspace path
      let _ = update_agent_workspace(agents_dir, agent_id, ws.path)

      // Run the agent
      run_agent_in_workspace_with_output(
        ws,
        task,
        agent_id,
        pipe_path,
        agents_dir,
      )

      // Clean up workspace
      case workspace.destroy(ws) {
        Ok(_) -> Nil
        Error(err) ->
          write_to_pipe(
            pipe_path,
            "Warning: Failed to cleanup workspace: " <> err,
          )
      }
    }
  }
}

/// Run agent in workspace with output to pipe
fn run_agent_in_workspace_with_output(
  ws: Workspace,
  task: Task,
  agent_id: String,
  pipe_path: String,
  agents_dir: String,
) -> Nil {
  write_to_pipe(pipe_path, "=== Agent " <> agent_id <> " ===")
  write_to_pipe(pipe_path, "Task: " <> task.title)
  write_to_pipe(pipe_path, "Workspace: " <> ws.path)
  write_to_pipe(pipe_path, "")

  // Get change ID
  case jj.get_current_change(ws.path) {
    Error(err) -> {
      write_to_pipe(pipe_path, "Error getting change ID: " <> err)
      mark_agent_failed(agents_dir, agent_id, "VCS error: " <> err)
    }

    Ok(change_id) -> {
      // Update jj description
      let _ = jj.describe(ws.path, "WIP: " <> jj.sanitize_title(task.title))

      // Create agent config for this run
      let agent_config = make_agent_config(agent_id, ws.path)

      // Create driver and build command
      let drv = claude.new()
      let command = driver.build_command(drv, task, agent_config)

      write_to_pipe(pipe_path, "Running: " <> command.executable)
      write_to_pipe(pipe_path, "")

      // Run command with output to pipe
      case
        run_command_with_pipe_output(
          command.executable,
          command.args,
          command.working_dir,
          pipe_path,
          agent_config.timeout_ms,
        )
      {
        #(exit_code, output) -> {
          let agent_result = driver.detect_result(drv, output, exit_code)

          // Update jj with result
          let _ = jj.describe_task_completion(ws.path, task, agent_result)

          case agent_result {
            driver.Success(_) -> {
              write_to_pipe(pipe_path, "")
              write_to_pipe(pipe_path, "=== Completed successfully ===")
              write_to_pipe(pipe_path, "Change ID: " <> change_id)
              mark_agent_idle(agents_dir, agent_id)
            }
            driver.Failure(reason, _) -> {
              write_to_pipe(pipe_path, "")
              write_to_pipe(pipe_path, "=== Failed ===")
              write_to_pipe(pipe_path, "Reason: " <> reason)
              mark_agent_failed(agents_dir, agent_id, reason)
            }
            driver.ContextExhausted(_) -> {
              write_to_pipe(pipe_path, "")
              write_to_pipe(pipe_path, "=== Context exhausted ===")
              mark_agent_idle(agents_dir, agent_id)
            }
            driver.Interrupted -> {
              write_to_pipe(pipe_path, "")
              write_to_pipe(pipe_path, "=== Interrupted ===")
              mark_agent_failed(agents_dir, agent_id, "Interrupted")
            }
          }
        }
      }
    }
  }
}

/// Mark agent as idle on disk
fn mark_agent_idle(agents_dir: String, agent_id: String) -> Nil {
  case agents.update_status(agents_dir, agent_id, types.Idle) {
    Ok(_) -> Nil
    Error(err) -> {
      io.println("Warning: Failed to mark agent idle: " <> err)
      Nil
    }
  }
}

/// Mark agent as failed on disk
fn mark_agent_failed(
  agents_dir: String,
  agent_id: String,
  reason: String,
) -> Nil {
  case agents.update_status(agents_dir, agent_id, types.Failed(reason)) {
    Ok(_) -> Nil
    Error(err) -> {
      io.println("Warning: Failed to mark agent failed: " <> err)
      Nil
    }
  }
}

/// Update agent workspace path on disk
fn update_agent_workspace(
  agents_dir: String,
  agent_id: String,
  workspace_path: String,
) -> Result(Nil, String) {
  case agents.load_agent(agents_dir, agent_id) {
    Error(err) -> Error(err)
    Ok(agent) -> {
      let updated = agents.AgentState(..agent, workspace_path: workspace_path)
      agents.save_agent(agents_dir, updated)
    }
  }
}

/// Write a line to the pipe (non-blocking, ignores errors)
fn write_to_pipe(pipe_path: String, message: String) -> Nil {
  // Use shell to write to pipe with timeout to prevent blocking
  let _ =
    run_shell_command(
      "sh",
      ["-c", "echo '" <> escape_for_shell(message) <> "' >> " <> pipe_path],
      ".",
      pipe_write_timeout_ms,
    )
  Nil
}

/// Escape string for shell single quotes
fn escape_for_shell(s: String) -> String {
  // Replace single quotes with escaped version
  string.replace(s, "'", "'\\''")
}

/// Generate a unique agent ID
fn generate_agent_id(task_id: String) -> String {
  let timestamp = erlang_system_time_ms()
  "agent-" <> task_id <> "-" <> int.to_string(timestamp)
}

/// Run a command with output going to a pipe
/// Returns (exit_code, captured_output)
fn run_command_with_pipe_output(
  executable: String,
  args: List(String),
  working_dir: String,
  pipe_path: String,
  timeout_ms: Int,
) -> #(Int, String) {
  // Build command that tees output to the pipe
  // Using shell to redirect and capture output
  let args_str =
    args
    |> join_with_spaces

  let cmd = executable <> " " <> args_str <> " 2>&1 | tee -a " <> pipe_path

  run_shell_command("sh", ["-c", cmd], working_dir, timeout_ms)
}

/// Join strings with spaces, quoting each argument
fn join_with_spaces(strs: List(String)) -> String {
  strs
  |> list.map(quote_arg)
  |> string.join(" ")
}

/// Quote an argument for shell if needed
fn quote_arg(s: String) -> String {
  "'" <> escape_for_shell(s) <> "'"
}

/// Run a shell command with timeout
/// Returns (exit_code, output)
@external(erlang, "scherzo_background_ffi", "run_command")
fn run_shell_command(
  executable: String,
  args: List(String),
  working_dir: String,
  timeout_ms: Int,
) -> #(Int, String)

/// Spawn an Erlang process with crash protection
/// If the work function crashes, on_error is called with the error message
@external(erlang, "scherzo_background_ffi", "safe_spawn")
fn safe_spawn(work: fn() -> Nil, on_error: fn(String) -> Nil) -> process.Pid

/// Get system time in milliseconds
@external(erlang, "scherzo_background_ffi", "system_time_ms")
fn erlang_system_time_ms() -> Int

/// Timeout for pipe write operations (ms)
const pipe_write_timeout_ms = 5000

/// Default number of retries for agent operations
const default_max_retries = 3

/// Create an AgentConfig with standard defaults
fn make_agent_config(agent_id: String, working_dir: String) -> types.AgentConfig {
  AgentConfig(
    id: agent_id,
    provider: Claude,
    working_dir: working_dir,
    max_retries: default_max_retries,
    timeout_ms: default_timeout_ms,
  )
}
