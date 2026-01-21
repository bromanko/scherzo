/// Control pane command implementations
///
/// Provides command handlers for the REPL that query task state
/// directly from the ticket system (single source of truth).
///
/// Info commands (status, tasks, agents) expose shared functions
/// that can be called by both CLI and REPL.
import gleam/int
import gleam/list
import gleam/string
import scherzo/core/task.{
  type Priority, type Task, Assigned, Blocked, Completed, Critical, Failed, High,
  InProgress, Low, Normal, Pending, Ready,
}
import scherzo/task/sources/ticket
import scherzo/ui/repl.{type CommandHandler, CommandError, CommandOutput}

/// Context for command execution
/// Uses the ticket system as the source of truth for tasks
pub type CommandContext {
  CommandContext(
    /// Directory containing .tickets/ for this project
    working_dir: String,
  )
}

// ---------------------------------------------------------------------------
// Shared Info Functions (used by both CLI and REPL)
// ---------------------------------------------------------------------------

/// Get status output - shared by CLI and REPL
pub fn get_status(working_dir: String) -> Result(String, String) {
  let tickets_dir = ticket.default_tickets_dir(working_dir)
  let task_source = ticket.new(tickets_dir)

  case task_source.fetch_tasks() {
    Error(err) -> Error("Failed to fetch tasks: " <> err)
    Ok(tasks) -> {
      let #(pending, in_progress, blocked, completed, failed) =
        count_task_statuses(tasks)

      let output =
        string.join(
          [
            "=== Scherzo Status ===",
            "",
            "Tasks:",
            "  Pending:     " <> int.to_string(pending),
            "  In Progress: " <> int.to_string(in_progress),
            "  Blocked:     " <> int.to_string(blocked),
            "  Completed:   " <> int.to_string(completed),
            "  Failed:      " <> int.to_string(failed),
            "  Total:       " <> int.to_string(list.length(tasks)),
          ],
          "\n",
        )

      Ok(output)
    }
  }
}

/// Get tasks list output - shared by CLI and REPL
pub fn get_tasks(working_dir: String) -> Result(String, String) {
  let tickets_dir = ticket.default_tickets_dir(working_dir)
  let task_source = ticket.new(tickets_dir)

  case task_source.fetch_tasks() {
    Error(err) -> Error("Failed to fetch tasks: " <> err)
    Ok(tasks) -> {
      case tasks {
        [] -> Ok("No tasks found in " <> tickets_dir)
        _ -> {
          let #(in_progress, pending, blocked, completed, failed) =
            group_tasks_by_status(tasks)

          // Format each group and filter out empty ones
          let groups = [
            format_task_group("In Progress", in_progress),
            format_task_group("Pending", pending),
            format_task_group("Blocked", blocked),
            format_task_group("Completed", completed),
            format_task_group("Failed", failed),
          ]

          let output =
            groups
            |> list.filter(fn(s) { s != "" })
            |> string.join("\n\n")

          Ok(output)
        }
      }
    }
  }
}

/// Get agents output - shared by CLI and REPL
pub fn get_agents() -> Result(String, String) {
  Ok(
    "No agents currently running.\n"
    <> "\n"
    <> "Agents are spawned when tasks are executed via 'scherzo run'.\n"
    <> "Multi-agent support coming in Phase 5.",
  )
}

// ---------------------------------------------------------------------------
// REPL Command Handlers
// ---------------------------------------------------------------------------

/// Create status command handler for REPL
pub fn status_command(ctx: CommandContext) -> CommandHandler {
  fn(_args) {
    case get_status(ctx.working_dir) {
      Ok(output) -> CommandOutput(output)
      Error(err) -> CommandError(err)
    }
  }
}

/// Create tasks command handler for REPL
pub fn tasks_command(ctx: CommandContext) -> CommandHandler {
  fn(_args) {
    case get_tasks(ctx.working_dir) {
      Ok(output) -> CommandOutput(output)
      Error(err) -> CommandError(err)
    }
  }
}

/// Create agents command handler for REPL
pub fn agents_command(_ctx: CommandContext) -> CommandHandler {
  fn(_args) {
    case get_agents() {
      Ok(output) -> CommandOutput(output)
      Error(err) -> CommandError(err)
    }
  }
}

// ---------------------------------------------------------------------------
// Helper Functions
// ---------------------------------------------------------------------------

/// Group tasks by status category in a single pass
/// Returns: #(in_progress, pending, blocked, completed, failed)
fn group_tasks_by_status(
  tasks: List(Task),
) -> #(List(Task), List(Task), List(Task), List(Task), List(Task)) {
  let #(in_progress, pending, blocked, completed, failed) =
    list.fold(tasks, #([], [], [], [], []), fn(acc, task) {
      let #(ip, pend, blk, comp, fail) = acc
      case task.status {
        Assigned(_) | InProgress(_, _) -> #([task, ..ip], pend, blk, comp, fail)
        Pending | Ready -> #(ip, [task, ..pend], blk, comp, fail)
        Blocked(_) -> #(ip, pend, [task, ..blk], comp, fail)
        Completed(_, _) -> #(ip, pend, blk, [task, ..comp], fail)
        Failed(_, _, _) -> #(ip, pend, blk, comp, [task, ..fail])
      }
    })
  // Reverse to maintain original order
  #(
    list.reverse(in_progress),
    list.reverse(pending),
    list.reverse(blocked),
    list.reverse(completed),
    list.reverse(failed),
  )
}

/// Format a group of tasks with a header
fn format_task_group(title: String, tasks: List(Task)) -> String {
  case tasks {
    [] -> ""
    _ -> {
      let count = list.length(tasks)
      let header = "=== " <> title <> " (" <> int.to_string(count) <> ") ==="
      // Sort by priority (critical first) before formatting
      let sorted =
        list.sort(tasks, fn(a, b) {
          int.compare(
            priority_sort_key(a.priority),
            priority_sort_key(b.priority),
          )
        })
      let lines = list.map(sorted, format_task_line_simple)
      header <> "\n" <> string.join(lines, "\n")
    }
  }
}

/// Format a task line with priority indicator
fn format_task_line_simple(task: Task) -> String {
  let priority_indicator = format_priority(task.priority)
  let id_short = string.slice(task.id, 0, 8)
  priority_indicator <> " " <> id_short <> " " <> task.title
}

/// Format priority as visual indicator
/// Critical: !!!, High: !!, Normal: (space), Low: .
fn format_priority(priority: Priority) -> String {
  case priority {
    Critical -> "!!!"
    High -> "!! "
    Normal -> "   "
    Low -> " . "
  }
}

/// Get sort key for priority (lower = higher priority)
fn priority_sort_key(priority: Priority) -> Int {
  case priority {
    Critical -> 0
    High -> 1
    Normal -> 2
    Low -> 3
  }
}

/// Count tasks by status category
/// Returns: #(pending, in_progress, blocked, completed, failed)
fn count_task_statuses(tasks: List(Task)) -> #(Int, Int, Int, Int, Int) {
  list.fold(tasks, #(0, 0, 0, 0, 0), fn(acc, task) {
    let #(pending, in_progress, blocked, completed, failed) = acc
    case task.status {
      Pending | Ready -> #(pending + 1, in_progress, blocked, completed, failed)
      Assigned(_) | InProgress(_, _) -> #(
        pending,
        in_progress + 1,
        blocked,
        completed,
        failed,
      )
      Blocked(_) -> #(pending, in_progress, blocked + 1, completed, failed)
      Completed(_, _) -> #(pending, in_progress, blocked, completed + 1, failed)
      Failed(_, _, _) -> #(pending, in_progress, blocked, completed, failed + 1)
    }
  })
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
