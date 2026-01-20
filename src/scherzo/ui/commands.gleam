/// Control pane command implementations
///
/// Provides command handlers for the REPL that query orchestrator state
/// and manage agent lifecycle.
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import scherzo/core/task.{
  type Task, type TaskStatus, Assigned, Blocked, Completed, Failed, InProgress,
  Pending, Ready,
}
import scherzo/core/types.{type AgentStatus}
import scherzo/state/store.{type AgentState, type Message as StoreMessage}
import scherzo/ui/repl.{
  type CommandHandler, type CommandResult, CommandError, CommandOutput,
}

/// Context for command execution
/// Contains references to state and orchestrator actors
pub type CommandContext {
  CommandContext(
    /// State store for querying tasks and agents
    store: Subject(StoreMessage),
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
  let tasks = store.get_all_tasks(ctx.store)
  let agents = store.get_all_agents(ctx.store)

  // Count tasks by status
  let #(pending, in_progress, completed, failed) = count_task_statuses(tasks)

  // Count active agents
  let active_agents =
    agents
    |> list.filter(fn(a) {
      case a.status {
        types.Running(_, _) -> True
        _ -> False
      }
    })
    |> list.length

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
    <> "\n"
    <> "Agents:\n"
    <> "  Total:  "
    <> int.to_string(list.length(agents))
    <> "\n"
    <> "  Active: "
    <> int.to_string(active_agents)

  CommandOutput(output)
}

/// Count tasks by status category
fn count_task_statuses(
  tasks: List(Task),
) -> #(Int, Int, Int, Int) {
  list.fold(tasks, #(0, 0, 0, 0), fn(acc, task) {
    let #(pending, in_progress, completed, failed) = acc
    case task.status {
      Pending | Ready | Blocked(_) -> #(pending + 1, in_progress, completed, failed)
      Assigned(_) | InProgress(_, _) -> #(pending, in_progress + 1, completed, failed)
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
  let tasks = store.get_all_tasks(ctx.store)

  case tasks {
    [] -> CommandOutput("No tasks found.")
    _ -> {
      let lines =
        tasks
        |> list.map(format_task_line)
        |> string.join("\n")

      CommandOutput("=== Tasks ===\n\n" <> lines)
    }
  }
}

/// Format a single task for display
fn format_task_line(task: Task) -> String {
  let status_str = format_task_status(task.status)
  let id_short = string.slice(task.id, 0, 8)

  id_short
  <> " ["
  <> status_str
  <> "] "
  <> task.title
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
pub fn agents_command(ctx: CommandContext) -> CommandHandler {
  fn(_args) { execute_agents(ctx) }
}

/// Execute agents command - show agent status
fn execute_agents(ctx: CommandContext) -> CommandResult {
  let agents = store.get_all_agents(ctx.store)

  case agents {
    [] -> CommandOutput("No agents registered.")
    _ -> {
      let lines =
        agents
        |> list.map(format_agent_line)
        |> string.join("\n")

      CommandOutput("=== Agents ===\n\n" <> lines)
    }
  }
}

/// Format a single agent for display
fn format_agent_line(agent: AgentState) -> String {
  let status_str = format_agent_status(agent.status)
  let id_short = string.slice(agent.config.id, 0, 12)

  id_short <> " [" <> status_str <> "]"
}

/// Format agent status for display
fn format_agent_status(status: AgentStatus) -> String {
  case status {
    types.Idle -> "idle"
    types.Running(task_id, _) -> "running:" <> string.slice(task_id, 0, 8)
    types.Failed(reason) -> "failed: " <> string.slice(reason, 0, 15)
  }
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
