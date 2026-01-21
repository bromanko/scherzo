/// Control pane command implementations
///
/// Provides command handlers for the REPL that query task state
/// directly from the ticket system (single source of truth).
import gleam/int
import gleam/list
import gleam/string
import scherzo/core/task.{
  type Task, type TaskStatus, Assigned, Blocked, Completed, Failed, InProgress,
  Pending, Ready,
}
import scherzo/task/sources/ticket
import scherzo/ui/repl.{
  type CommandHandler, type CommandResult, CommandError, CommandOutput,
}

/// Context for command execution
/// Uses the ticket system as the source of truth for tasks
pub type CommandContext {
  CommandContext(
    /// Directory containing .tickets/ for this project
    working_dir: String,
  )
}

// ---------------------------------------------------------------------------
// Status Commands (s-8acb)
// ---------------------------------------------------------------------------

/// Create status command handler
pub fn status_command(ctx: CommandContext) -> CommandHandler {
  fn(_args) { execute_status(ctx) }
}

/// Execute status command - show overall system status
fn execute_status(ctx: CommandContext) -> CommandResult {
  let tickets_dir = ticket.default_tickets_dir(ctx.working_dir)
  let task_source = ticket.new(tickets_dir)

  case task_source.fetch_tasks() {
    Error(err) -> CommandError("Failed to fetch tasks: " <> err)
    Ok(tasks) -> {
      // Count tasks by status
      let #(pending, in_progress, completed, failed) = count_task_statuses(tasks)

      let output =
        "=== Scherzo Status ===\n"
        <> "\n"
        <> "Tasks:\n"
        <> "  Pending:     "
        <> int.to_string(pending)
        <> "\n"
        <> "  In Progress: "
        <> int.to_string(in_progress)
        <> "\n"
        <> "  Completed:   "
        <> int.to_string(completed)
        <> "\n"
        <> "  Failed:      "
        <> int.to_string(failed)
        <> "\n"
        <> "  Total:       "
        <> int.to_string(list.length(tasks))

      CommandOutput(output)
    }
  }
}

/// Count tasks by status category
fn count_task_statuses(tasks: List(Task)) -> #(Int, Int, Int, Int) {
  list.fold(tasks, #(0, 0, 0, 0), fn(acc, task) {
    let #(pending, in_progress, completed, failed) = acc
    case task.status {
      Pending | Ready | Blocked(_) -> #(
        pending + 1,
        in_progress,
        completed,
        failed,
      )
      Assigned(_) | InProgress(_, _) -> #(
        pending,
        in_progress + 1,
        completed,
        failed,
      )
      Completed(_, _) -> #(pending, in_progress, completed + 1, failed)
      Failed(_, _, _) -> #(pending, in_progress, completed, failed + 1)
    }
  })
}

/// Create tasks command handler
pub fn tasks_command(ctx: CommandContext) -> CommandHandler {
  fn(_args) { execute_tasks(ctx) }
}

/// Execute tasks command - list all tasks with status
fn execute_tasks(ctx: CommandContext) -> CommandResult {
  let tickets_dir = ticket.default_tickets_dir(ctx.working_dir)
  let task_source = ticket.new(tickets_dir)

  case task_source.fetch_tasks() {
    Error(err) -> CommandError("Failed to fetch tasks: " <> err)
    Ok(tasks) -> {
      case tasks {
        [] -> CommandOutput("No tasks found in " <> tickets_dir)
        _ -> {
          let lines =
            tasks
            |> list.map(format_task_line)
            |> string.join("\n")

          CommandOutput("=== Tasks ===\n\n" <> lines)
        }
      }
    }
  }
}

/// Format a single task for display
fn format_task_line(task: Task) -> String {
  let status_str = format_task_status(task.status)
  let id_short = string.slice(task.id, 0, 8)

  id_short <> " [" <> status_str <> "] " <> task.title
}

/// Format task status for display
fn format_task_status(status: TaskStatus) -> String {
  case status {
    Pending -> "pending"
    Ready -> "ready"
    Blocked(reason) -> "blocked: " <> string.slice(reason, 0, 20)
    Assigned(agent_id) -> "assigned:" <> string.slice(agent_id, 0, 8)
    InProgress(agent_id, _) -> "running:" <> string.slice(agent_id, 0, 8)
    Completed(_, _) -> "done"
    Failed(_, reason, _) -> "failed: " <> string.slice(reason, 0, 15)
  }
}

/// Create agents command handler
pub fn agents_command(_ctx: CommandContext) -> CommandHandler {
  fn(_args) { execute_agents() }
}

/// Execute agents command - show agent status
/// Note: Agent tracking is runtime-only, managed by the orchestrator during task execution.
/// This will show active agents once multi-agent orchestration is implemented (Phase 5).
fn execute_agents() -> CommandResult {
  CommandOutput(
    "No agents currently running.\n"
    <> "\n"
    <> "Agents are spawned when tasks are executed via 'scherzo run'.\n"
    <> "Multi-agent support coming in Phase 5.",
  )
}

// ---------------------------------------------------------------------------
// Lifecycle Commands (s-6013)
// ---------------------------------------------------------------------------

/// Create pause command handler (stub - will be wired to orchestrator)
pub fn pause_command(_ctx: CommandContext) -> CommandHandler {
  fn(args) {
    case args {
      [agent_id] -> {
        // TODO: Wire to orchestrator pause functionality
        CommandOutput("Pause requested for agent: " <> agent_id)
      }
      _ -> CommandError("Usage: pause <agent-id>")
    }
  }
}

/// Create resume command handler (stub - will be wired to orchestrator)
pub fn resume_command(_ctx: CommandContext) -> CommandHandler {
  fn(args) {
    case args {
      [agent_id] -> {
        // TODO: Wire to orchestrator resume functionality
        CommandOutput("Resume requested for agent: " <> agent_id)
      }
      _ -> CommandError("Usage: resume <agent-id>")
    }
  }
}

/// Create retry command handler (stub - will be wired to orchestrator)
pub fn retry_command(_ctx: CommandContext) -> CommandHandler {
  fn(args) {
    case args {
      [task_id] -> {
        // TODO: Wire to orchestrator retry functionality
        CommandOutput("Retry requested for task: " <> task_id)
      }
      _ -> CommandError("Usage: retry <task-id>")
    }
  }
}

/// Create kill command handler (stub - will be wired to orchestrator)
pub fn kill_command(_ctx: CommandContext) -> CommandHandler {
  fn(args) {
    case args {
      [agent_id] -> {
        // TODO: Wire to orchestrator kill functionality
        CommandOutput("Kill requested for agent: " <> agent_id)
      }
      _ -> CommandError("Usage: kill <agent-id>")
    }
  }
}

// ---------------------------------------------------------------------------
// Focus Commands (s-2449)
// ---------------------------------------------------------------------------

/// Create focus command handler (stub - will be wired to layout)
pub fn focus_command(_ctx: CommandContext) -> CommandHandler {
  fn(args) {
    case args {
      [agent_id] -> {
        // TODO: Wire to layout focus functionality
        CommandOutput("Focus requested for agent: " <> agent_id)
      }
      _ -> CommandError("Usage: focus <agent-id>")
    }
  }
}

/// Create abort command handler - immediate shutdown
pub fn abort_command(_ctx: CommandContext) -> CommandHandler {
  fn(_args) {
    // TODO: Wire to orchestrator for immediate shutdown
    CommandOutput("Abort requested - shutting down immediately")
  }
}

// ---------------------------------------------------------------------------
// Command Registration
// ---------------------------------------------------------------------------

/// Register all informational commands (status, tasks, agents)
pub fn register_info_commands(
  config: repl.ReplConfig,
  ctx: CommandContext,
) -> repl.ReplConfig {
  config
  |> repl.add_command("status", status_command(ctx))
  |> repl.add_command("tasks", tasks_command(ctx))
  |> repl.add_command("agents", agents_command(ctx))
}

/// Register lifecycle commands (pause, resume, retry, kill)
pub fn register_lifecycle_commands(
  config: repl.ReplConfig,
  ctx: CommandContext,
) -> repl.ReplConfig {
  config
  |> repl.add_command("pause", pause_command(ctx))
  |> repl.add_command("resume", resume_command(ctx))
  |> repl.add_command("retry", retry_command(ctx))
  |> repl.add_command("kill", kill_command(ctx))
}

/// Register focus and abort commands
pub fn register_focus_commands(
  config: repl.ReplConfig,
  ctx: CommandContext,
) -> repl.ReplConfig {
  config
  |> repl.add_command("focus", focus_command(ctx))
  |> repl.add_command("abort", abort_command(ctx))
}

/// Register all control commands
pub fn register_all_commands(
  config: repl.ReplConfig,
  ctx: CommandContext,
) -> repl.ReplConfig {
  config
  |> register_info_commands(ctx)
  |> register_lifecycle_commands(ctx)
  |> register_focus_commands(ctx)
}
