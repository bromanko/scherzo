import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glint
import scherzo/agent/checkpoint
import scherzo/agent/workspace
import scherzo/orchestrator
import scherzo/task/sources/ticket
import scherzo/ui/runner
import scherzo/ui/tmux
import simplifile

pub fn main() {
  glint.new()
  |> glint.with_name("scherzo")
  |> glint.pretty_help(glint.default_pretty_help())
  |> glint.add(at: [], do: root_command())
  |> glint.add(at: ["run"], do: run_command())
  |> glint.add(at: ["status"], do: status_command())
  |> glint.add(at: ["prime"], do: prime_command())
  |> glint.add(at: ["checkpoint"], do: checkpoint_command())
  |> glint.add(at: ["attach"], do: attach_command())
  |> glint.add(at: ["repl"], do: repl_command())
  |> glint.run(argv.load().arguments)
}

fn root_command() -> glint.Command(Nil) {
  use <- glint.command_help("Scherzo - AI Agent Orchestrator")
  use _, _, _ <- glint.command()
  io.println("Scherzo - AI Agent Orchestrator")
  io.println("Use 'scherzo --help' for usage information")
  Nil
}

/// Flag for working directory
fn workdir_flag() {
  glint.string_flag("workdir")
  |> glint.flag_default(".")
  |> glint.flag_help("Working directory for the agent")
}

/// Flag for running from tickets
fn from_tickets_flag() {
  glint.bool_flag("from-tickets")
  |> glint.flag_default(False)
  |> glint.flag_help("Process tasks from .tickets/ directory")
}

/// Flag for max tasks to process
fn max_tasks_flag() {
  glint.int_flag("max-tasks")
  |> glint.flag_default(0)
  |> glint.flag_help("Maximum tasks to process (0 = unlimited)")
}

fn run_command() -> glint.Command(Nil) {
  use <- glint.command_help(
    "Run tasks. Usage: scherzo run \"title\" \"desc\" OR scherzo run --from-tickets",
  )
  use workdir_getter <- glint.flag(workdir_flag())
  use from_tickets_getter <- glint.flag(from_tickets_flag())
  use max_tasks_getter <- glint.flag(max_tasks_flag())
  use _, args, flags <- glint.command()

  // Get flags (flag_default ensures these always succeed, unwrap provides defense-in-depth)
  let working_dir =
    workdir_getter(flags)
    |> result.unwrap(".")
    |> resolve_path()

  let from_tickets = from_tickets_getter(flags) |> result.unwrap(False)
  let max_tasks = max_tasks_getter(flags) |> result.unwrap(0)

  // Check if running from tickets
  case from_tickets {
    True -> run_from_tickets(working_dir, max_tasks)
    False -> {
      // Parse arguments for single task
      case args {
        [title, description] -> run_single_task(working_dir, title, description)
        [title] -> run_single_task(working_dir, title, title)
        _ -> {
          io.println("Usage: scherzo run \"task title\" [\"task description\"]")
          io.println("       scherzo run --from-tickets [--max-tasks N]")
          io.println("")
          io.println("Examples:")
          io.println("  scherzo run \"Fix the login bug\"")
          io.println(
            "  scherzo run \"Add feature\" \"Add user authentication to the API\"",
          )
          io.println("  scherzo run --from-tickets")
          io.println("  scherzo run --from-tickets --max-tasks 5")
        }
      }
    }
  }
  Nil
}

/// Run a single task
fn run_single_task(working_dir: String, title: String, description: String) {
  io.println("Running task: " <> title)
  io.println("Working directory: " <> working_dir)
  io.println("")

  let config = orchestrator.default_config(working_dir)

  case orchestrator.run_task(config, title, description) {
    orchestrator.RunSuccess(output, change_id) -> {
      io.println("Task completed successfully!")
      io.println("Change ID: " <> change_id)
      io.println("")
      io.println("Output:")
      io.println(output)
    }
    orchestrator.RunFailed(reason) -> {
      io.println("Task failed: " <> reason)
    }
    orchestrator.RunExhausted(continuations, last_output, change_id) -> {
      io.println(
        "Task exhausted after "
        <> int.to_string(continuations)
        <> " continuations",
      )
      io.println("Change ID: " <> change_id)
      io.println("")
      io.println("Last output:")
      io.println(last_output)
    }
  }
}

/// Run tasks from tickets directory
fn run_from_tickets(working_dir: String, max_tasks: Int) {
  io.println("Processing tickets from: " <> working_dir <> "/.tickets/")
  io.println("")

  let config = orchestrator.default_config(working_dir)
  let tickets_dir = ticket.default_tickets_dir(working_dir)
  let task_source = ticket.new(tickets_dir)

  case orchestrator.run_from_source(config, task_source, max_tasks) {
    Error(err) -> {
      io.println("Error: " <> err)
    }
    Ok(batch_result) -> {
      io.println("")
      io.println("=== Results ===")
      io.println(
        "Total: "
        <> int.to_string(batch_result.total)
        <> " | Completed: "
        <> int.to_string(list.length(batch_result.completed))
        <> " | Failed: "
        <> int.to_string(list.length(batch_result.failed)),
      )

      case batch_result.failed {
        [] -> Nil
        failures -> {
          io.println("")
          io.println("Failed tasks:")
          list.each(failures, fn(r) {
            io.println("  - " <> r.title <> " (" <> r.task_id <> ")")
          })
        }
      }
    }
  }
}

fn status_command() -> glint.Command(Nil) {
  use <- glint.command_help("Show orchestrator status")
  use _, _, _ <- glint.command()
  io.println("Status: Ready")
  io.println("Available agents: claude")
  Nil
}

/// Resolve a path (handle relative paths)
fn resolve_path(path: String) -> String {
  case string.starts_with(path, "/") {
    True -> path
    False -> {
      let cwd = get_cwd()
      case path {
        "." -> cwd
        _ -> cwd <> "/" <> path
      }
    }
  }
}

/// Get current working directory
fn get_cwd() -> String {
  case simplifile.current_directory() {
    Ok(dir) -> dir
    Error(_) -> "."
  }
}

/// Prime command - inject context for an agent (called by SessionStart hook)
fn prime_command() -> glint.Command(Nil) {
  use <- glint.command_help(
    "Inject task context for an agent. Called by SessionStart hook.",
  )
  use _, args, _ <- glint.command()

  let cwd = get_cwd()

  // Try to read task info from current directory (workspace)
  case workspace.read_task_info(cwd) {
    Ok(task_info) -> {
      // Output context to stdout for hook injection
      io.println("# Scherzo Agent Context")
      io.println("")
      io.println(
        "You are a Scherzo-managed agent working on the following task.",
      )
      io.println("")
      io.println("## Task: " <> task_info.title)
      io.println("")
      io.println(task_info.description)
      io.println("")
      io.println("## Guidelines")
      io.println("")
      io.println("- Focus only on this task")
      io.println("- Make changes in the current working directory")
      io.println("- When complete, summarize what you accomplished")
      io.println("- If you cannot complete the task, explain why")
      io.println("")
      io.println("Task ID: " <> task_info.id)
    }
    Error(_) -> {
      // Not in a workspace, check if task_id was passed as argument
      case args {
        [task_id] -> {
          io.println("# Scherzo Agent Context")
          io.println("")
          io.println("Task ID: " <> task_id)
          io.println("")
          io.println(
            "Note: Could not load full task info. Running in standalone mode.",
          )
        }
        _ -> {
          io.println("# Scherzo Agent Context")
          io.println("")
          io.println(
            "Note: No workspace detected. Running without task context.",
          )
        }
      }
    }
  }
  Nil
}

/// Flag for checkpoint type
fn checkpoint_type_flag() {
  glint.string_flag("type")
  |> glint.flag_default("final")
  |> glint.flag_help(
    "Type of checkpoint: incremental, pre_compact, or final (default: final)",
  )
}

/// Checkpoint command - save agent state (called by Stop hook)
fn checkpoint_command() -> glint.Command(Nil) {
  use <- glint.command_help(
    "Save checkpoint state for an agent. Called by Stop hook.",
  )
  use type_getter <- glint.flag(checkpoint_type_flag())
  use _, args, flags <- glint.command()

  let cwd = get_cwd()

  // Get checkpoint type from flag (flag_default ensures this always succeeds)
  let checkpoint_type =
    type_getter(flags)
    |> result.unwrap("final")
    |> parse_checkpoint_type()

  // Try to read task info from current directory (workspace)
  case workspace.read_task_info(cwd) {
    Ok(task_info) -> {
      create_checkpoint(task_info, checkpoint_type, cwd)
    }
    Error(_) -> {
      case args {
        [task_id] -> {
          // Minimal checkpoint without full task info
          let task_info =
            workspace.TaskInfo(
              id: task_id,
              title: "Unknown task",
              description: "",
              repo_dir: cwd,
            )
          create_checkpoint(task_info, checkpoint_type, cwd)
        }
        _ -> {
          io.println("Warning: No task context found for checkpoint")
          io.println("Usage: scherzo checkpoint [--type=final] [task-id]")
        }
      }
    }
  }
  Nil
}

/// Parse checkpoint type string to enum
fn parse_checkpoint_type(type_str: String) -> checkpoint.CheckpointType {
  case type_str {
    "incremental" -> checkpoint.Incremental
    "pre_compact" -> checkpoint.PreCompact
    _ -> checkpoint.Final
  }
}

/// Create and save a checkpoint
fn create_checkpoint(
  task_info: workspace.TaskInfo,
  checkpoint_type: checkpoint.CheckpointType,
  _workspace_dir: String,
) -> Nil {
  // Get agent ID from environment (optional - default "agent-1" is safe for single-agent use)
  let agent_id = get_env("SCHERZO_AGENT_ID") |> result.unwrap("agent-1")

  // Create checkpoint config using the repo_dir from task info
  let config = checkpoint.default_config(task_info.repo_dir)

  // For final checkpoints, we would ideally read work summary from stdin
  // For now, use a placeholder that indicates what the agent was working on
  let work_summary = case checkpoint_type {
    checkpoint.Final -> "Agent completed work on task: " <> task_info.title
    checkpoint.PreCompact ->
      "Context compaction checkpoint for: " <> task_info.title
    checkpoint.Incremental -> "Incremental checkpoint for: " <> task_info.title
  }

  // Next steps only for final checkpoints
  let next_steps = case checkpoint_type {
    checkpoint.Final -> Some("Review changes and verify task completion")
    _ -> None
  }

  // Create the checkpoint
  case
    checkpoint.create(
      config,
      task_info.id,
      agent_id,
      checkpoint_type,
      work_summary,
      next_steps,
    )
  {
    Ok(cp) -> {
      let type_name = case checkpoint_type {
        checkpoint.Incremental -> "Incremental"
        checkpoint.PreCompact -> "PreCompact"
        checkpoint.Final -> "Final"
      }
      io.println(
        type_name
        <> " checkpoint saved for task: "
        <> task_info.id
        <> " (seq: "
        <> int.to_string(cp.sequence)
        <> ")",
      )
      io.println("Change ID: " <> cp.jj_change_id)
      io.println(
        "Files modified: " <> int.to_string(list.length(cp.files_modified)),
      )
    }
    Error(err) -> {
      io.println("Failed to create checkpoint: " <> err)
    }
  }
}

/// Get environment variable
@external(erlang, "os", "getenv")
fn os_getenv(name: String) -> Result(String, Nil)

fn get_env(name: String) -> Result(String, Nil) {
  os_getenv(name)
}

// ---------------------------------------------------------------------------
// Attach Command
// ---------------------------------------------------------------------------

fn attach_command() -> glint.Command(Nil) {
  use <- glint.command_help("Attach to a running scherzo tmux session")
  use _, _, _ <- glint.command()

  let session_name = tmux.default_session_name

  case tmux.session_exists(session_name) {
    False -> {
      io.println("No scherzo session found.")
      io.println("")
      io.println("Start a session with: scherzo run --from-tickets")
      io.println("Or create one manually: tmux new-session -s " <> session_name)
    }
    True -> {
      io.println("Attaching to scherzo session...")
      case tmux.attach_session(session_name) {
        Ok(_) -> Nil
        Error(tmux.SessionNotFound(_)) -> {
          io.println("Error: Session disappeared before attach")
        }
        Error(tmux.CommandFailed(msg)) -> {
          io.println("Error attaching to session: " <> msg)
        }
        Error(tmux.SessionExists(_)) -> {
          // This shouldn't happen for attach
          io.println("Unexpected error: session exists")
        }
        Error(tmux.TmuxNotAvailable) -> {
          io.println("Error: tmux is not available on this system")
        }
      }
    }
  }
  Nil
}

// ---------------------------------------------------------------------------
// REPL Command
// ---------------------------------------------------------------------------

fn repl_command() -> glint.Command(Nil) {
  use <- glint.command_help("Start the interactive control REPL")
  use workdir_getter <- glint.flag(workdir_flag())
  use _, _, flags <- glint.command()

  let working_dir =
    workdir_getter(flags)
    |> result.unwrap(".")
    |> resolve_path()

  io.println("Starting scherzo REPL...")
  io.println("Working directory: " <> working_dir)
  io.println("")

  case runner.start_standalone(working_dir) {
    Ok(_) -> io.println("Goodbye!")
    Error(runner.StoreError(msg)) -> io.println("Error starting store: " <> msg)
    Error(runner.SessionError(msg)) -> io.println("Error with session: " <> msg)
  }

  Nil
}
