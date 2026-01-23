/// Control pane command implementations
///
/// Provides command handlers for the REPL that query task state
/// directly from the ticket system (single source of truth).
///
/// Info commands (status, tasks, agents) expose shared functions
/// that can be called by both CLI and REPL.
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import scherzo/agent/background
import scherzo/core/names
import scherzo/core/task.{
  type Priority, type Task, Assigned, Blocked, Completed, Critical, Epic, Failed,
  High, InProgress, Low, Normal, Pending, Ready,
}
import scherzo/core/types
import scherzo/state/agents as agents_state
import scherzo/task/source
import scherzo/task/sources/ticket
import scherzo/ui/repl.{type CommandHandler, CommandError, CommandOutput}
import scherzo/ui/session_manager.{type SessionManager}

/// Context for command execution
/// Uses the ticket system as the source of truth for tasks
pub type CommandContext {
  CommandContext(
    /// Directory containing .tickets/ for this project
    working_dir: String,
    /// Optional session manager for tmux pane creation
    session_manager: Option(SessionManager),
    /// Command to invoke scherzo (for hooks)
    /// Development: "cd /path/to/scherzo && gleam run --"
    /// Production: "scherzo"
    scherzo_bin: String,
  )
}

/// Filter options for tasks command
pub type TasksFilter {
  TasksFilter(
    /// Show all tasks including completed (overrides other filters)
    show_all: Bool,
    /// Show only specific status categories
    status_filter: StatusFilter,
    /// Show tasks in tree view (by parent/child hierarchy)
    show_tree: Bool,
  )
}

/// Status filter options
pub type StatusFilter {
  /// Show actionable tasks (pending, in-progress, blocked, failed) - default
  ShowActionable
  /// Show only pending tasks
  ShowPending
  /// Show only in-progress tasks
  ShowInProgress
  /// Show only completed tasks
  ShowCompleted
  /// Show only blocked tasks
  ShowBlocked
  /// Show only failed tasks
  ShowFailed
}

/// Default filter - shows actionable tasks (hides completed)
pub fn default_filter() -> TasksFilter {
  TasksFilter(show_all: False, status_filter: ShowActionable, show_tree: False)
}

/// Parse filter arguments from command line args
pub fn parse_filter_args(args: List(String)) -> TasksFilter {
  list.fold(args, default_filter(), fn(filter, arg) {
    case arg {
      "--all" -> TasksFilter(..filter, show_all: True)
      "--pending" -> TasksFilter(..filter, status_filter: ShowPending)
      "--in-progress" -> TasksFilter(..filter, status_filter: ShowInProgress)
      "--completed" -> TasksFilter(..filter, status_filter: ShowCompleted)
      "--blocked" -> TasksFilter(..filter, status_filter: ShowBlocked)
      "--failed" -> TasksFilter(..filter, status_filter: ShowFailed)
      "--tree" -> TasksFilter(..filter, show_tree: True)
      _ -> filter
    }
  })
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
/// Use default_filter() for standard behavior (hides completed)
pub fn get_tasks_filtered(
  working_dir: String,
  filter: TasksFilter,
) -> Result(String, String) {
  let tickets_dir = ticket.default_tickets_dir(working_dir)
  let task_source = ticket.new(tickets_dir)

  case task_source.fetch_tasks() {
    Error(err) -> Error("Failed to fetch tasks: " <> err)
    Ok(tasks) -> {
      case tasks {
        [] -> Ok("No tasks found in " <> tickets_dir)
        _ -> {
          // Apply status filtering first
          let filtered_tasks = filter_tasks_by_status(tasks, filter)

          // Tree view or grouped view?
          case filter.show_tree {
            True -> Ok(format_tree_view(filtered_tasks))
            False -> Ok(format_grouped_view(tasks, filter))
          }
        }
      }
    }
  }
}

/// Filter tasks by status according to filter settings
fn filter_tasks_by_status(tasks: List(Task), filter: TasksFilter) -> List(Task) {
  case filter.show_all {
    True -> tasks
    False ->
      list.filter(tasks, fn(t) {
        status_matches_filter(t.status, filter.status_filter)
      })
  }
}

/// Check if a task status matches the given filter
fn status_matches_filter(status: task.TaskStatus, filter: StatusFilter) -> Bool {
  case filter {
    ShowActionable ->
      case status {
        Completed(_, _) -> False
        _ -> True
      }
    ShowPending ->
      case status {
        Pending | Ready -> True
        _ -> False
      }
    ShowInProgress ->
      case status {
        Assigned(_) | InProgress(_, _) -> True
        _ -> False
      }
    ShowCompleted ->
      case status {
        Completed(_, _) -> True
        _ -> False
      }
    ShowBlocked ->
      case status {
        Blocked(_) -> True
        _ -> False
      }
    ShowFailed ->
      case status {
        Failed(_, _, _) -> True
        _ -> False
      }
  }
}

/// Format tasks in grouped view (by status)
fn format_grouped_view(tasks: List(Task), filter: TasksFilter) -> String {
  let #(in_progress, pending, blocked, completed, failed) =
    group_tasks_by_status(tasks)

  // Determine which groups to show based on filter
  // Pass all tasks to blocked group for resolving blocker statuses
  let #(groups, hidden_count) = case filter.show_all {
    True -> {
      // Show all groups
      #(
        [
          format_task_group("In Progress", in_progress),
          format_task_group("Pending", pending),
          format_task_group_with_context("Blocked", blocked, tasks),
          format_task_group("Completed", completed),
          format_task_group("Failed", failed),
        ],
        0,
      )
    }
    False ->
      case filter.status_filter {
        ShowActionable -> {
          // Show actionable tasks, hide completed
          let completed_count = list.length(completed)
          #(
            [
              format_task_group("In Progress", in_progress),
              format_task_group("Pending", pending),
              format_task_group_with_context("Blocked", blocked, tasks),
              format_task_group("Failed", failed),
            ],
            completed_count,
          )
        }
        ShowPending -> #([format_task_group("Pending", pending)], 0)
        ShowInProgress -> #([format_task_group("In Progress", in_progress)], 0)
        ShowCompleted -> #([format_task_group("Completed", completed)], 0)
        ShowBlocked -> #(
          [format_task_group_with_context("Blocked", blocked, tasks)],
          0,
        )
        ShowFailed -> #([format_task_group("Failed", failed)], 0)
      }
  }

  let filtered_output =
    groups
    |> list.filter(fn(s) { s != "" })
    |> string.join("\n\n")

  // Add hidden count message if applicable
  case hidden_count > 0 {
    True ->
      filtered_output
      <> "\n\n"
      <> int.to_string(hidden_count)
      <> " completed tasks hidden. Use --all to show."
    False -> filtered_output
  }
}

/// Format tasks in tree view (by parent/child hierarchy)
fn format_tree_view(tasks: List(Task)) -> String {
  case tasks {
    [] -> "No tasks match the filter."
    _ -> {
      let header = "=== Task Hierarchy ==="
      let tree_lines = build_tree_lines(tasks)
      header <> "\n" <> string.join(tree_lines, "\n")
    }
  }
}

/// Build tree lines from a flat list of tasks
fn build_tree_lines(tasks: List(Task)) -> List(String) {
  // Find root tasks (no parent or parent not in list)
  let task_ids = list.map(tasks, fn(t) { t.id })
  let roots =
    list.filter(tasks, fn(t) {
      case t.parent {
        None -> True
        Some(parent_id) -> !list.contains(task_ids, parent_id)
      }
    })

  // Sort roots by priority
  let sorted_roots =
    list.sort(roots, fn(a, b) {
      int.compare(priority_sort_key(a.priority), priority_sort_key(b.priority))
    })

  // Render each root and its children
  let root_count = list.length(sorted_roots)
  sorted_roots
  |> list.index_map(fn(root, idx) {
    let is_last = idx == root_count - 1
    render_task_tree(root, tasks, "", is_last)
  })
  |> list.flatten
}

/// Recursively render a task and its children with tree lines
fn render_task_tree(
  task: Task,
  all_tasks: List(Task),
  prefix: String,
  is_last: Bool,
) -> List(String) {
  // Tree branch characters
  let branch = case is_last {
    True -> "└── "
    False -> "├── "
  }

  // Epics get a box icon, regular tasks get priority indicator
  let type_indicator = case task.task_type {
    Epic -> "📦 "
    _ -> format_priority(task.priority) <> " "
  }
  let id_short = string.slice(task.id, 0, 8)
  let status_indicator = format_status_indicator(task.status)
  let line =
    prefix
    <> branch
    <> type_indicator
    <> id_short
    <> " "
    <> status_indicator
    <> task.title

  // Find children of this task
  let children =
    list.filter(all_tasks, fn(t) {
      case t.parent {
        Some(parent_id) -> parent_id == task.id
        None -> False
      }
    })

  // Sort children by priority
  let sorted_children =
    list.sort(children, fn(a, b) {
      int.compare(priority_sort_key(a.priority), priority_sort_key(b.priority))
    })

  // Build prefix for children (continue line or space)
  let child_prefix = case is_last {
    True -> prefix <> "    "
    False -> prefix <> "│   "
  }

  // Render children recursively
  let child_count = list.length(sorted_children)
  let child_lines =
    sorted_children
    |> list.index_map(fn(child, idx) {
      let child_is_last = idx == child_count - 1
      render_task_tree(child, all_tasks, child_prefix, child_is_last)
    })
    |> list.flatten

  [line, ..child_lines]
}

/// Format a brief status indicator for tree view (markdown-style)
fn format_status_indicator(status: task.TaskStatus) -> String {
  case status {
    Pending -> "[ ] "
    Ready -> "[ ] "
    Assigned(_) -> "[>] "
    InProgress(_, _) -> "[>] "
    Completed(_, _) -> "[x] "
    Failed(_, _, _) -> "[!] "
    Blocked(_) -> "[!] "
  }
}

/// Get tasks list output with default filter (hides completed)
/// Convenience wrapper for backward compatibility
pub fn get_tasks(working_dir: String) -> Result(String, String) {
  get_tasks_filtered(working_dir, default_filter())
}

/// Get agents output - shared by CLI and REPL
pub fn get_agents(working_dir: String) -> Result(String, String) {
  let agents_dir = working_dir <> "/" <> agents_state.default_agents_dir
  let all_agents = agents_state.load_all_agents(agents_dir)

  case all_agents {
    [] ->
      Ok(
        "No agents currently running.\n"
        <> "\n"
        <> "Use 'run \"task title\"' or 'run --from-tickets' to spawn agents.",
      )
    _ -> Ok(format_agents_list(all_agents))
  }
}

/// Format a list of agents for display
fn format_agents_list(all_agents: List(agents_state.AgentState)) -> String {
  let header =
    "=== Agents (" <> int.to_string(list.length(all_agents)) <> ") ==="
  let lines =
    list.map(all_agents, fn(agent) {
      let status_str = format_agent_status(agent.status)
      // Generate human-readable name from agent ID
      let friendly_name = names.from_agent_id(agent.config.id)
      let provider = format_provider(agent.config.provider)
      status_str <> " " <> friendly_name <> " [" <> provider <> "]"
    })
  header <> "\n" <> string.join(lines, "\n")
}

/// Format agent status for display
fn format_agent_status(status: types.AgentStatus) -> String {
  case status {
    types.Idle -> "⏸  Idle   "
    types.Running(_, _) -> "▶  Running"
    types.Completed(_, _) -> "✓  Done   "
    types.Failed(reason) -> "✗  Failed  " <> string.slice(reason, 0, 15)
  }
}

/// Format provider name
fn format_provider(provider: types.AgentProvider) -> String {
  case provider {
    types.Claude -> "claude"
    types.Codex -> "codex"
    types.Gemini -> "gemini"
  }
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
/// Supports: tasks, tasks --all, tasks --pending, tasks --in-progress, etc.
pub fn tasks_command(ctx: CommandContext) -> CommandHandler {
  fn(args) {
    let filter = parse_filter_args(args)
    case get_tasks_filtered(ctx.working_dir, filter) {
      Ok(output) -> CommandOutput(output)
      Error(err) -> CommandError(err)
    }
  }
}

/// Create agents command handler for REPL
pub fn agents_command(ctx: CommandContext) -> CommandHandler {
  fn(_args) {
    case get_agents(ctx.working_dir) {
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

/// Format priority as visual indicator
/// Critical: !!!, High: !!, Normal: (space), Low: ↓
fn format_priority(priority: Priority) -> String {
  case priority {
    Critical -> "!!!"
    High -> "!! "
    Normal -> "   "
    Low -> " ↓ "
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

/// Format a status as a brief string for display
fn format_status_brief(status: task.TaskStatus) -> String {
  case status {
    Pending -> "pending"
    Ready -> "ready"
    Assigned(_) -> "assigned"
    InProgress(_, _) -> "in_progress"
    Completed(_, _) -> "completed"
    Failed(_, _, _) -> "failed"
    Blocked(_) -> "blocked"
  }
}

/// Check if a string looks like a task ID
fn is_task_id(s: String) -> Bool {
  string.starts_with(s, "s-")
  || string.starts_with(s, "t-")
  || string.starts_with(s, "task-")
}

/// Find a task by ID in a list
fn find_task(id: String, tasks: List(Task)) -> Result(Task, Nil) {
  list.find(tasks, fn(t) { t.id == id })
}

/// Format the blocking reason, resolving task references if possible
fn format_block_reason(reason: String, all_tasks: List(Task)) -> String {
  // Check if reason looks like a task ID (starts with s- or similar pattern)
  case is_task_id(reason), find_task(reason, all_tasks) {
    True, Ok(blocker_task) -> {
      let status_str = format_status_brief(blocker_task.status)
      "    Blocked by: " <> reason <> " (" <> status_str <> ")"
    }
    _, _ -> "    Blocked by: " <> reason
  }
}

/// Format a blocked task with visual indicator and reason
fn format_blocked_task_line(
  id: String,
  title: String,
  reason: String,
  all_tasks: List(Task),
) -> String {
  let header = "⛔  " <> id <> " " <> title
  let reason_line = format_block_reason(reason, all_tasks)
  header <> "\n" <> reason_line
}

/// Format a task line with status-aware formatting
fn format_task_line(task: Task, all_tasks: List(Task)) -> String {
  let id_short = string.slice(task.id, 0, 8)
  case task.status {
    Blocked(reason) ->
      format_blocked_task_line(id_short, task.title, reason, all_tasks)
    _ -> {
      let priority_indicator = format_priority(task.priority)
      priority_indicator <> " " <> id_short <> " " <> task.title
    }
  }
}

/// Format a group of tasks with optional context for resolving blockers
fn format_task_group_with_context(
  title: String,
  tasks: List(Task),
  all_tasks: List(Task),
) -> String {
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
      let lines =
        list.map(sorted, fn(task) { format_task_line(task, all_tasks) })
      header <> "\n" <> string.join(lines, "\n")
    }
  }
}

/// Format a group of tasks with a header
fn format_task_group(title: String, tasks: List(Task)) -> String {
  format_task_group_with_context(title, tasks, [])
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
// Run Command (s-xxxx)
// ---------------------------------------------------------------------------

/// Create run command handler - enqueue and execute tasks
pub fn run_command(ctx: CommandContext) -> CommandHandler {
  fn(args) {
    case parse_run_args(args) {
      RunSingleTask(title, description) ->
        run_single_task(ctx, title, description)
      RunFromTickets(max_tasks) -> run_from_tickets(ctx, max_tasks)
      RunShowUsage -> CommandOutput(run_usage())
    }
  }
}

/// Parsed run command arguments
type RunArgs {
  RunSingleTask(title: String, description: String)
  RunFromTickets(max_tasks: Int)
  RunShowUsage
}

/// Parse run command arguments
fn parse_run_args(args: List(String)) -> RunArgs {
  case args {
    // run --from-tickets [--max-tasks N]
    ["--from-tickets", ..rest] -> {
      let max_tasks = parse_max_tasks(rest)
      RunFromTickets(max_tasks)
    }
    // run "title" "description"
    [title, description] -> RunSingleTask(title, description)
    // run "title" (description defaults to title)
    [title] -> RunSingleTask(title, title)
    // No args or invalid
    _ -> RunShowUsage
  }
}

/// Parse --max-tasks N from remaining args
fn parse_max_tasks(args: List(String)) -> Int {
  case args {
    ["--max-tasks", n, ..] ->
      int.parse(n)
      |> result.unwrap(0)
    _ -> 0
  }
}

/// Run a single task (spawns agent in background)
fn run_single_task(
  ctx: CommandContext,
  title: String,
  description: String,
) -> repl.CommandResult {
  // Generate a task ID
  let task_id = "task-" <> int.to_string(erlang_system_time())

  // Create the task
  let new_task = task.new(task_id, title, description)

  // Spawn agent in background
  case
    background.spawn_agent(
      ctx.working_dir,
      new_task,
      ctx.session_manager,
      ctx.scherzo_bin,
    )
  {
    Error(reason) -> CommandError("Failed to spawn agent: " <> reason)
    Ok(#(spawn_result, _updated_manager)) -> {
      case spawn_result {
        background.SpawnSuccess(agent_id, _pipe_path) -> {
          let friendly_name = names.from_agent_id(agent_id)
          CommandOutput(
            "Spawned agent "
            <> friendly_name
            <> " [task]\n"
            <> "ID: "
            <> agent_id
            <> "\n\n"
            <> "Use 'agents' to check status.",
          )
        }
        background.SpawnFailed(reason) ->
          CommandError("Failed to spawn agent: " <> reason)
      }
    }
  }
}

@external(erlang, "erlang", "system_time")
fn erlang_system_time() -> Int

/// Run tasks from tickets directory (spawns agents in background)
fn run_from_tickets(ctx: CommandContext, max_tasks: Int) -> repl.CommandResult {
  let tickets_dir = ticket.default_tickets_dir(ctx.working_dir)
  let task_source = ticket.new(tickets_dir)

  // Handle explicit 0 request early
  case max_tasks {
    0 ->
      CommandOutput(
        "No tasks spawned (--max-tasks 0).\nUse --max-tasks N to spawn agents.",
      )
    _ -> {
      // Get ready tasks from source
      case source.get_ready_tasks(task_source) {
        Error(err) -> CommandError("Failed to get ready tasks: " <> err)
        Ok(tasks) -> {
          // Limit to max_tasks if specified (negative means all)
          let tasks_to_run = case max_tasks {
            n if n > 0 -> list.take(tasks, n)
            _ -> tasks
          }

          case tasks_to_run {
            [] -> CommandOutput("No ready tasks found in tickets.")
            _ -> {
              // Spawn agents in background
              let spawn_results =
                spawn_agents_for_tasks(
                  ctx.working_dir,
                  tasks_to_run,
                  ctx.session_manager,
                  ctx.scherzo_bin,
                )

              let spawned_agents =
                list.filter_map(spawn_results, fn(r) {
                  case r {
                    background.SpawnSuccess(agent_id, _) -> Ok(agent_id)
                    background.SpawnFailed(_) -> Error(Nil)
                  }
                })

              let spawned_count = list.length(spawned_agents)
              let failed_count = list.length(spawn_results) - spawned_count

              // Format spawned agent names
              let agent_lines =
                list.map(spawned_agents, fn(agent_id) {
                  let friendly_name = names.from_agent_id(agent_id)
                  "  " <> friendly_name <> " [task]"
                })

              let result =
                string.join(
                  [
                    "Spawned " <> int.to_string(spawned_count) <> " agent(s):",
                    ..agent_lines
                  ],
                  "\n",
                )
                <> "\n"
                <> case failed_count > 0 {
                  True ->
                    "\n" <> int.to_string(failed_count) <> " failed to spawn."
                  False -> ""
                }
                <> "\n\nUse 'agents' to check status."

              CommandOutput(result)
            }
          }
        }
      }
    }
  }
}

/// Spawn agents for a list of tasks
fn spawn_agents_for_tasks(
  working_dir: String,
  tasks: List(Task),
  session_manager: Option(SessionManager),
  scherzo_bin: String,
) -> List(background.SpawnResult) {
  // Spawn agents, threading the session_manager through
  let #(results, _final_manager) =
    list.fold(tasks, #([], session_manager), fn(acc, task) {
      let #(results_so_far, current_manager) = acc
      case
        background.spawn_agent(working_dir, task, current_manager, scherzo_bin)
      {
        Error(reason) -> #(
          [background.SpawnFailed(reason), ..results_so_far],
          current_manager,
        )
        Ok(#(spawn_result, updated_manager)) -> #(
          [spawn_result, ..results_so_far],
          updated_manager,
        )
      }
    })
  list.reverse(results)
}

/// Usage text for run command
fn run_usage() -> String {
  string.join(
    [
      "Usage: run \"task title\" [\"task description\"]",
      "       run --from-tickets [--max-tasks N]",
      "",
      "Examples:",
      "  run \"Fix the login bug\"",
      "  run \"Add feature\" \"Add user authentication to the API\"",
      "  run --from-tickets",
      "  run --from-tickets --max-tasks 5",
    ],
    "\n",
  )
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

/// Register run command
pub fn register_run_commands(
  config: repl.ReplConfig,
  ctx: CommandContext,
) -> repl.ReplConfig {
  config
  |> repl.add_command("run", run_command(ctx))
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
  |> register_run_commands(ctx)
}
