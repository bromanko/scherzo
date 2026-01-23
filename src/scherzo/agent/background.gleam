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

/// Spawn an agent to work on a task
///
/// When a session_manager is provided, runs Claude interactively in a tmux pane.
/// Otherwise, falls back to background mode with pipe-based output.
pub fn spawn_agent(
  working_dir: String,
  task: Task,
  session_manager: Option(SessionManager),
  scherzo_bin: String,
) -> Result(#(SpawnResult, Option(SessionManager)), String) {
  let agents_dir = working_dir <> "/" <> agents.default_agents_dir
  let agent_id = generate_agent_id(task.id)

  case session_manager {
    // Interactive mode: run Claude directly in tmux pane
    Some(manager) ->
      spawn_agent_interactive(
        working_dir,
        task,
        agent_id,
        manager,
        agents_dir,
        scherzo_bin,
      )
    // Background mode: run with pipe-based output
    None ->
      spawn_agent_background(
        working_dir,
        task,
        agent_id,
        agents_dir,
        scherzo_bin,
      )
  }
}

/// Spawn an interactive agent in a tmux pane
fn spawn_agent_interactive(
  working_dir: String,
  task: Task,
  agent_id: String,
  manager: SessionManager,
  agents_dir: String,
  scherzo_bin: String,
) -> Result(#(SpawnResult, Option(SessionManager)), String) {
  let started_at = erlang_system_time_ms()
  let workspace_config =
    workspace.default_config_with_scherzo_bin(working_dir, scherzo_bin)

  // Load custom agent config
  case agent_config.load_task_config(working_dir) {
    Error(err) -> Error("Invalid agent config: " <> err)
    Ok(custom_config) -> {
      // Create workspace for the agent
      case
        workspace.create(workspace_config, task, agent_id, Some(custom_config))
      {
        Error(err) -> Error("Failed to create workspace: " <> err)
        Ok(ws) -> {
          // Build the claude command (interactive mode)
          let agent_cfg = make_agent_config(agent_id, ws.path, True)
          let drv = claude.new()
          let command = driver.build_command(drv, task, agent_cfg)

          // Build the full shell command with PATH setup
          let shell_cmd = build_interactive_command(command, ws.path)

          // Register agent state
          let agent_state =
            agents.AgentState(
              config: agent_cfg,
              status: types.Running(task.id, started_at),
              task_id: task.id,
              workspace_path: ws.path,
              pipe_path: "",
            )
          let _ = agents.save_agent(agents_dir, agent_state)

          // Create interactive tmux pane running claude
          case
            session_manager.add_agent_interactive(manager, agent_id, shell_cmd)
          {
            Error(_) -> {
              io.println(
                "Warning: Failed to create interactive pane for agent "
                <> agent_id,
              )
              Ok(#(SpawnSuccess(agent_id, ""), Some(manager)))
            }
            Ok(new_manager) -> {
              Ok(#(SpawnSuccess(agent_id, ""), Some(new_manager)))
            }
          }
        }
      }
    }
  }
}

/// Build the shell command for interactive mode
fn build_interactive_command(
  command: driver.Command,
  workspace_path: String,
) -> String {
  let extra_paths =
    "/.sprite/languages/node/nvm/versions/node/v22.20.0/bin:/home/sprite/.local/bin:/home/sprite/.nix-profile/bin"

  let args_str = command.args |> join_with_spaces

  "export PATH=\""
  <> extra_paths
  <> ":$PATH\" && cd "
  <> quote_arg(workspace_path)
  <> " && "
  <> command.executable
  <> " "
  <> args_str
}

/// Spawn agent in background mode (no tmux, pipe-based output)
fn spawn_agent_background(
  working_dir: String,
  task: Task,
  agent_id: String,
  agents_dir: String,
  scherzo_bin: String,
) -> Result(#(SpawnResult, Option(SessionManager)), String) {
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
      let started_at = erlang_system_time_ms()
      // Background mode - non-interactive
      let agent_cfg = make_agent_config(agent_id, working_dir, False)

      let agent_state =
        agents.AgentState(
          config: agent_cfg,
          status: types.Running(task.id, started_at),
          task_id: task.id,
          workspace_path: "",
          pipe_path: pipe_path,
        )
      let _ = agents.save_agent(agents_dir, agent_state)

      // Spawn background process
      safe_spawn(
        fn() {
          run_agent_background(
            working_dir,
            task,
            agent_id,
            pipe_path,
            agents_dir,
            scherzo_bin,
          )
        },
        fn(error_msg) {
          write_to_pipe(pipe_path, "=== Process Crashed ===")
          write_to_pipe(pipe_path, error_msg)
          mark_agent_failed(agents_dir, agent_id, "Process crashed")
        },
      )

      Ok(#(SpawnSuccess(agent_id, pipe_path), None))
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
  scherzo_bin: String,
) -> Nil {
  let workspace_config =
    workspace.default_config_with_scherzo_bin(working_dir, scherzo_bin)

  // Chain the setup steps, collecting error context
  let setup_result = {
    use custom_config <- result.try(
      agent_config.load_task_config(working_dir)
      |> result.map_error(fn(e) { "Config error: " <> e }),
    )
    use ws <- result.try(
      workspace.create(workspace_config, task, agent_id, Some(custom_config))
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

      // Create agent config for this run (background mode - non-interactive)
      let agent_config = make_agent_config(agent_id, ws.path, False)

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
  // Note: must use absolute path for spawn_executable
  let _ =
    run_shell_command(
      "/bin/sh",
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
  let args_str =
    args
    |> join_with_spaces

  // Extend PATH to include common locations where claude might be installed
  // This avoids using a login shell which can produce noisy output from .bashrc
  let extra_paths =
    "/.sprite/languages/node/nvm/versions/node/v22.20.0/bin:/home/sprite/.local/bin:/home/sprite/.nix-profile/bin"

  // Use script to allocate a PTY, which forces unbuffered/streaming output
  // The -q flag suppresses script's own messages
  // We use double quotes for the outer command and escape the inner command
  let inner_cmd = executable <> " " <> args_str
  let escaped_inner = string.replace(inner_cmd, "\"", "\\\"")

  let cmd =
    "export PATH=\""
    <> extra_paths
    <> ":$PATH\" && cd "
    <> quote_arg(working_dir)
    <> " && script -q -c \""
    <> escaped_inner
    <> "\" /dev/null 2>&1 | tee -a "
    <> pipe_path

  // Use plain bash (not login shell) to avoid .bashrc noise
  // Note: must use absolute path for spawn_executable
  run_shell_command("/bin/bash", ["-c", cmd], working_dir, timeout_ms)
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
fn make_agent_config(
  agent_id: String,
  working_dir: String,
  interactive: Bool,
) -> types.AgentConfig {
  AgentConfig(
    id: agent_id,
    provider: Claude,
    working_dir: working_dir,
    max_retries: default_max_retries,
    timeout_ms: default_timeout_ms,
    interactive: interactive,
  )
}
