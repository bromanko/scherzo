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
import scherzo/cli/init
import scherzo/core/types
import scherzo/orchestrator
import scherzo/orchestrator/coordinator
import scherzo/state/agents
import scherzo/task/sources/ticket
import scherzo/ui/commands
import scherzo/ui/layout
import scherzo/ui/runner
import scherzo/ui/tmux
import simplifile

pub fn main() {
  glint.new()
  |> glint.with_name("scherzo")
  |> glint.pretty_help(glint.default_pretty_help())
  |> glint.add(at: [], do: root_command())
  |> glint.add(at: ["init"], do: init.command())
  |> glint.add(at: ["run"], do: run_command())
  |> glint.add(at: ["status"], do: status_command())
  |> glint.add(at: ["tasks"], do: tasks_command())
  |> glint.add(at: ["agents"], do: agents_command())
  |> glint.add(at: ["prime"], do: prime_command())
  |> glint.add(at: ["checkpoint"], do: checkpoint_command())
  |> glint.add(at: ["attach"], do: attach_command())
  |> glint.add(at: ["repl"], do: repl_command())
  |> glint.add(at: ["console"], do: console_command())
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
/// Note: No default so we can distinguish explicit from omitted
fn workdir_flag() {
  glint.string_flag("workdir")
  |> glint.flag_help(
    "Working directory for the agent (default: current directory)",
  )
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

  // Get flags
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
    coordinator.RunSuccess(output, change_id) -> {
      io.println("Task completed successfully!")
      io.println("Change ID: " <> change_id)
      io.println("")
      io.println("Output:")
      io.println(output)
    }
    coordinator.RunFailed(reason) -> {
      io.println("Task failed: " <> reason)
    }
    coordinator.RunExhausted(continuations, last_output, change_id) -> {
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
    coordinator.RunGatesFailed(gate_name, feedback_summary) -> {
      io.println("Gates failed: " <> gate_name)
      io.println("Feedback: " <> feedback_summary)
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
  use <- glint.command_help("Show task status summary")
  use workdir_getter <- glint.flag(workdir_flag())
  use _, _, flags <- glint.command()

  get_working_dir(workdir_getter, flags)
  |> commands.get_status
  |> print_result
}

/// Flag for showing all tasks
fn all_flag() {
  glint.bool_flag("all")
  |> glint.flag_default(False)
  |> glint.flag_help("Show all tasks including completed")
}

/// Flag for showing only pending tasks
fn pending_flag() {
  glint.bool_flag("pending")
  |> glint.flag_default(False)
  |> glint.flag_help("Show only pending tasks")
}

/// Flag for showing only in-progress tasks
fn in_progress_flag() {
  glint.bool_flag("in-progress")
  |> glint.flag_default(False)
  |> glint.flag_help("Show only in-progress tasks")
}

/// Flag for showing only completed tasks
fn completed_flag() {
  glint.bool_flag("completed")
  |> glint.flag_default(False)
  |> glint.flag_help("Show only completed tasks")
}

/// Flag for showing only blocked tasks
fn blocked_flag() {
  glint.bool_flag("blocked")
  |> glint.flag_default(False)
  |> glint.flag_help("Show only blocked tasks")
}

/// Flag for showing only failed tasks
fn failed_flag() {
  glint.bool_flag("failed")
  |> glint.flag_default(False)
  |> glint.flag_help("Show only failed tasks")
}

/// Flag for showing tasks in tree view
fn tree_flag() {
  glint.bool_flag("tree")
  |> glint.flag_default(False)
  |> glint.flag_help("Show tasks in hierarchy tree view")
}

fn tasks_command() -> glint.Command(Nil) {
  use <- glint.command_help(
    "List tasks grouped by status. Default: hides completed tasks.",
  )
  use workdir_getter <- glint.flag(workdir_flag())
  use all_getter <- glint.flag(all_flag())
  use pending_getter <- glint.flag(pending_flag())
  use in_progress_getter <- glint.flag(in_progress_flag())
  use completed_getter <- glint.flag(completed_flag())
  use blocked_getter <- glint.flag(blocked_flag())
  use failed_getter <- glint.flag(failed_flag())
  use tree_getter <- glint.flag(tree_flag())
  use _, _, flags <- glint.command()

  let working_dir = get_working_dir(workdir_getter, flags)

  // Get flag values
  let show_all = all_getter(flags) |> result.unwrap(False)
  let show_pending = pending_getter(flags) |> result.unwrap(False)
  let show_in_progress = in_progress_getter(flags) |> result.unwrap(False)
  let show_completed = completed_getter(flags) |> result.unwrap(False)
  let show_blocked = blocked_getter(flags) |> result.unwrap(False)
  let show_failed = failed_getter(flags) |> result.unwrap(False)
  let show_tree = tree_getter(flags) |> result.unwrap(False)

  // Build filter from flags (first matching filter wins)
  let status_filter = case
    show_pending,
    show_in_progress,
    show_completed,
    show_blocked,
    show_failed
  {
    True, _, _, _, _ -> commands.ShowPending
    _, True, _, _, _ -> commands.ShowInProgress
    _, _, True, _, _ -> commands.ShowCompleted
    _, _, _, True, _ -> commands.ShowBlocked
    _, _, _, _, True -> commands.ShowFailed
    _, _, _, _, _ -> commands.ShowActionable
  }

  let filter =
    commands.TasksFilter(
      show_all: show_all,
      status_filter: status_filter,
      show_tree: show_tree,
    )

  commands.get_tasks_filtered(working_dir, filter)
  |> print_result
}

fn agents_command() -> glint.Command(Nil) {
  use <- glint.command_help("Show agent status")
  use workdir_getter <- glint.flag(workdir_flag())
  use _, _, flags <- glint.command()

  let working_dir =
    workdir_getter(flags)
    |> result.unwrap(".")
    |> resolve_path()

  commands.get_agents(working_dir)
  |> print_result
}

/// Print a Result(String, String) - Ok prints output, Error prints error message
fn print_result(result: Result(String, String)) -> Nil {
  case result {
    Ok(output) -> io.println(output)
    Error(err) -> io.println("Error: " <> err)
  }
  Nil
}

/// Get working directory from flags, defaulting to current directory
fn get_working_dir(
  workdir_getter: fn(glint.Flags) -> Result(String, a),
  flags: glint.Flags,
) -> String {
  workdir_getter(flags)
  |> result.unwrap(".")
  |> resolve_path()
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

/// Flag for agent ID
fn agent_id_flag() {
  glint.string_flag("agent-id")
  |> glint.flag_default("")
  |> glint.flag_help(
    "The agent ID to update status for (optional, falls back to env var)",
  )
}

/// Checkpoint command - save agent state (called by Stop hook)
fn checkpoint_command() -> glint.Command(Nil) {
  use <- glint.command_help(
    "Save checkpoint state for an agent. Called by Stop hook.",
  )
  use type_getter <- glint.flag(checkpoint_type_flag())
  use agent_id_getter <- glint.flag(agent_id_flag())
  use _, args, flags <- glint.command()

  let cwd = get_cwd()

  // Get checkpoint type from flag (flag_default ensures this always succeeds)
  let checkpoint_type =
    type_getter(flags)
    |> result.unwrap("final")
    |> parse_checkpoint_type()

  // Get agent ID from flag, or fall back to environment variable
  let agent_id = case agent_id_getter(flags) |> result.unwrap("") {
    "" -> get_env("SCHERZO_AGENT_ID") |> result.unwrap("agent-1")
    id -> id
  }

  // Try to read task info from current directory (workspace)
  case workspace.read_task_info(cwd) {
    Ok(task_info) -> {
      create_checkpoint(task_info, checkpoint_type, agent_id)
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
          create_checkpoint(task_info, checkpoint_type, agent_id)
        }
        _ -> {
          io.println("Warning: No task context found for checkpoint")
          io.println(
            "Usage: scherzo checkpoint [--type=final] [--agent-id=ID] [task-id]",
          )
        }
      }
    }
  }
  Nil
}

/// Parse checkpoint type string to enum
pub fn parse_checkpoint_type(type_str: String) -> checkpoint.CheckpointType {
  case type_str {
    "incremental" -> checkpoint.Incremental
    "pre_compact" -> checkpoint.PreCompact
    "final" -> checkpoint.Final
    other -> {
      io.println(
        "Warning: Unknown checkpoint type '" <> other <> "', using 'final'",
      )
      checkpoint.Final
    }
  }
}

/// Create and save a checkpoint
fn create_checkpoint(
  task_info: workspace.TaskInfo,
  checkpoint_type: checkpoint.CheckpointType,
  agent_id: String,
) -> Nil {
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

      // For final checkpoints, mark the agent as completed
      case checkpoint_type {
        checkpoint.Final -> {
          let agents_dir = task_info.repo_dir <> "/.scherzo/agents"
          let completed_at = get_timestamp()
          let status = types.Completed(task_info.id, completed_at)
          // Use the agent_id from environment (already retrieved above)
          case agents.update_status(agents_dir, agent_id, status) {
            Ok(_) -> io.println("Agent marked as completed: " <> agent_id)
            Error(err) ->
              io.println("Warning: Failed to update agent status: " <> err)
          }
        }
        _ -> Nil
      }
    }
    Error(err) -> {
      io.println("Failed to create checkpoint: " <> err)
    }
  }
}

/// Get environment variable (returns empty string if not set)
@external(erlang, "scherzo_env_ffi", "getenv")
fn env_getenv(name: String) -> String

fn get_env(name: String) -> Result(String, Nil) {
  case env_getenv(name) {
    "" -> Error(Nil)
    value -> Ok(value)
  }
}

/// Get current timestamp in milliseconds
@external(erlang, "os", "system_time")
fn get_timestamp() -> Int

// ---------------------------------------------------------------------------
// Attach Helpers
// ---------------------------------------------------------------------------

/// Attach to a tmux session with standardized error handling
fn do_attach_session(session_name: String) -> Nil {
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
      io.println("Unexpected error: session exists")
    }
    Error(tmux.TmuxNotAvailable) -> {
      io.println("Error: tmux is not available on this system")
    }
  }
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
      io.println("Start a session with: scherzo console")
      io.println("Or create one manually: tmux new-session -s " <> session_name)
    }
    True -> do_attach_session(session_name)
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

  // Compute scherzo command for hooks (development mode)
  let scherzo_dir = get_cwd()
  let scherzo_bin = "cd " <> scherzo_dir <> " && gleam run --"

  // Check if we're running inside the scherzo tmux session
  let session_name = tmux.default_session_name
  let in_scherzo_session = tmux.is_inside_session(session_name)

  let result = case in_scherzo_session {
    True -> {
      // Attach to the existing session for agent pane management
      runner.start_in_session(session_name, working_dir, scherzo_bin)
    }
    False -> {
      // Run standalone without session management
      runner.start_standalone(working_dir)
    }
  }

  case result {
    Ok(_) -> io.println("Goodbye!")
    Error(runner.SessionError(msg)) -> io.println("Error: " <> msg)
  }

  Nil
}

// ---------------------------------------------------------------------------
// Console Command
// ---------------------------------------------------------------------------

fn console_command() -> glint.Command(Nil) {
  use <- glint.command_help(
    "Start the scherzo console (creates tmux session or attaches if exists)",
  )
  use workdir_getter <- glint.flag(workdir_flag())
  use _, _, flags <- glint.command()

  let session_name = tmux.default_session_name
  let workdir_flag_value = workdir_getter(flags)

  case tmux.session_exists(session_name) {
    True -> {
      // Session already exists, attach to it
      // Warn if --workdir was explicitly provided since it will be ignored
      case workdir_flag_value {
        Ok(_) -> {
          io.println("Note: --workdir ignored, attaching to existing session")
        }
        Error(_) -> Nil
      }
      do_attach_session(session_name)
    }
    False -> {
      // No session exists, create one with the REPL running inside tmux
      let working_dir =
        workdir_flag_value
        |> result.unwrap(".")
        |> resolve_path()

      // Build the command to run the REPL inside the tmux session
      // During development: gleam run -- repl --workdir=<dir>
      // In production: scherzo repl --workdir=<dir>
      let scherzo_dir = get_cwd()
      let repl_command =
        "cd "
        <> scherzo_dir
        <> " && gleam run -- repl --workdir="
        <> working_dir

      io.println("Starting scherzo console...")
      io.println("Working directory: " <> working_dir)

      // Create tmux session with REPL running in control pane
      case layout.create_session_with_command(session_name, repl_command) {
        Error(layout.TmuxError(tmux.TmuxNotAvailable)) -> {
          io.println("Error: tmux is not available on this system")
        }
        Error(layout.TmuxError(tmux.SessionExists(_))) -> {
          io.println("Error: Session already exists (race condition)")
        }
        Error(_) -> {
          io.println("Error: Failed to create tmux session")
        }
        Ok(_) -> {
          // Attach to the newly created session
          do_attach_session(session_name)
        }
      }
    }
  }

  Nil
}
