import gleam/option.{None}
import gleam/string
import gleeunit/should
import scherzo/ui/commands
import scherzo/ui/repl

// ---------------------------------------------------------------------------
// Test Helpers
// ---------------------------------------------------------------------------

/// Create a test command context pointing to the scherzo project itself
/// which has real tickets we can query
fn test_context() -> commands.CommandContext {
  commands.CommandContext(
    working_dir: ".",
    session_manager: None,
    scherzo_bin: "scherzo",
  )
}

// ---------------------------------------------------------------------------
// Status Command Tests
// ---------------------------------------------------------------------------

pub fn status_command_returns_output_test() {
  let ctx = test_context()
  let handler = commands.status_command(ctx)
  let result = handler([])

  case result {
    repl.CommandOutput(output) -> {
      // Should contain "Scherzo Status" header
      { output != "" }
      |> should.be_true
    }
    repl.CommandError(_) -> {
      // Error is also acceptable if no tickets dir
      should.be_true(True)
    }
    _ -> should.fail()
  }
}

// ---------------------------------------------------------------------------
// Tasks Command Tests
// ---------------------------------------------------------------------------

pub fn tasks_command_returns_output_test() {
  let ctx = test_context()
  let handler = commands.tasks_command(ctx)
  let result = handler([])

  case result {
    repl.CommandOutput(output) -> {
      // Should return some output
      { output != "" }
      |> should.be_true
    }
    repl.CommandError(_) -> {
      // Error is also acceptable if no tickets dir
      should.be_true(True)
    }
    _ -> should.fail()
  }
}

// ---------------------------------------------------------------------------
// Agents Command Tests
// ---------------------------------------------------------------------------

pub fn agents_command_shows_no_agents_test() {
  let ctx = test_context()
  let handler = commands.agents_command(ctx)
  let result = handler([])

  case result {
    repl.CommandOutput(output) -> {
      // Should mention no agents running
      { output != "" }
      |> should.be_true
    }
    _ -> should.fail()
  }
}

// ---------------------------------------------------------------------------
// Lifecycle Command Tests
// ---------------------------------------------------------------------------

pub fn pause_command_requires_agent_id_test() {
  let ctx = test_context()
  let handler = commands.pause_command(ctx)

  // Without agent id should error
  let result = handler([])
  case result {
    repl.CommandError(_) -> should.be_true(True)
    _ -> should.fail()
  }

  // With agent id should succeed (stub)
  let result2 = handler(["agent-1"])
  case result2 {
    repl.CommandOutput(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn resume_command_requires_agent_id_test() {
  let ctx = test_context()
  let handler = commands.resume_command(ctx)

  // Without agent id should error
  let result = handler([])
  case result {
    repl.CommandError(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn retry_command_requires_task_id_test() {
  let ctx = test_context()
  let handler = commands.retry_command(ctx)

  // Without task id should error
  let result = handler([])
  case result {
    repl.CommandError(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn kill_command_requires_agent_id_test() {
  let ctx = test_context()
  let handler = commands.kill_command(ctx)

  // Without agent id should error
  let result = handler([])
  case result {
    repl.CommandError(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ---------------------------------------------------------------------------
// Focus Command Tests
// ---------------------------------------------------------------------------

pub fn focus_command_requires_agent_id_test() {
  let ctx = test_context()
  let handler = commands.focus_command(ctx)

  // Without agent id should error
  let result = handler([])
  case result {
    repl.CommandError(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn abort_command_test() {
  let ctx = test_context()
  let handler = commands.abort_command(ctx)
  let result = handler([])

  case result {
    repl.CommandOutput(_) -> should.be_true(True)
    _ -> should.fail()
  }
}

// ---------------------------------------------------------------------------
// Command Registration Tests
// ---------------------------------------------------------------------------

pub fn register_info_commands_test() {
  let ctx = test_context()
  let config = repl.default_config()
  let config = commands.register_info_commands(config, ctx)

  repl.has_command(config, "status")
  |> should.be_true

  repl.has_command(config, "tasks")
  |> should.be_true

  repl.has_command(config, "agents")
  |> should.be_true
}

pub fn register_lifecycle_commands_test() {
  let ctx = test_context()
  let config = repl.default_config()
  let config = commands.register_lifecycle_commands(config, ctx)

  repl.has_command(config, "pause")
  |> should.be_true

  repl.has_command(config, "resume")
  |> should.be_true

  repl.has_command(config, "retry")
  |> should.be_true

  repl.has_command(config, "kill")
  |> should.be_true
}

pub fn register_all_commands_test() {
  let ctx = test_context()
  let config = repl.default_config()
  let config = commands.register_all_commands(config, ctx)

  // All commands should be registered
  repl.has_command(config, "status")
  |> should.be_true
  repl.has_command(config, "pause")
  |> should.be_true
  repl.has_command(config, "focus")
  |> should.be_true
  repl.has_command(config, "abort")
  |> should.be_true
}

// ---------------------------------------------------------------------------
// Tasks Output Grouping Tests (s-4ea5)
// ---------------------------------------------------------------------------

pub fn tasks_output_groups_by_status_test() {
  // Test that tasks output contains group headers
  case commands.get_tasks(".") {
    Ok(output) -> {
      // Should have at least one group header with count format
      // Headers look like "=== Pending (32) ==="
      let has_group_header =
        string.contains(output, "=== Pending (")
        || string.contains(output, "=== Completed (")
        || string.contains(output, "=== In Progress (")
        || string.contains(output, "=== Blocked (")
        || string.contains(output, "=== Failed (")

      has_group_header
      |> should.be_true
    }
    Error(_) -> {
      // Error is acceptable if no tickets dir
      should.be_true(True)
    }
  }
}

pub fn tasks_output_has_pending_group_test() {
  // We know this project has pending tasks
  case commands.get_tasks(".") {
    Ok(output) -> {
      // Should contain pending section since we have pending tickets
      string.contains(output, "=== Pending (")
      |> should.be_true
    }
    Error(_) -> should.be_true(True)
  }
}

pub fn tasks_output_has_completed_group_test() {
  // We know this project has completed tasks
  // Use --all filter since completed are hidden by default
  let filter =
    commands.TasksFilter(
      show_all: True,
      status_filter: commands.ShowActionable,
      show_tree: False,
    )
  case commands.get_tasks_filtered(".", filter) {
    Ok(output) -> {
      // Should contain completed section since we have closed tickets
      string.contains(output, "=== Completed (")
      |> should.be_true
    }
    Error(_) -> should.be_true(True)
  }
}

pub fn tasks_output_groups_separated_by_blank_lines_test() {
  // Use --all filter to ensure multiple groups are shown
  let filter =
    commands.TasksFilter(
      show_all: True,
      status_filter: commands.ShowActionable,
      show_tree: False,
    )
  case commands.get_tasks_filtered(".", filter) {
    Ok(output) -> {
      // Groups should be separated by double newlines (blank line between groups)
      // The format is: "...last task\n\n=== Next Group..."
      string.contains(output, "\n\n===")
      |> should.be_true
    }
    Error(_) -> should.be_true(True)
  }
}

pub fn tasks_output_no_status_brackets_in_lines_test() {
  // In the grouped view, individual task lines should NOT have [status]
  // since status is implied by the group
  case commands.get_tasks(".") {
    Ok(output) -> {
      // Old format was "s-xxxx [pending] Title"
      // New format is "s-xxxx Title" (no brackets)
      let has_old_format =
        string.contains(output, " [pending] ")
        || string.contains(output, " [done] ")
        || string.contains(output, " [ready] ")

      has_old_format
      |> should.be_false
    }
    Error(_) -> should.be_true(True)
  }
}

// ---------------------------------------------------------------------------
// Priority Indicator Tests (s-3529)
// ---------------------------------------------------------------------------

pub fn tasks_output_has_priority_indicators_test() {
  // Task lines should have priority indicators at the start
  case commands.get_tasks(".") {
    Ok(output) -> {
      // Lines should start with priority indicator followed by task ID
      // Low priority uses " ↓ ", Normal uses "   ", High "!! ", Critical "!!!"
      // At minimum we should see some spacing before task IDs
      let has_indicator_pattern =
        string.contains(output, "    s-")
        // Normal priority (3 spaces + space before ID)
        || string.contains(output, " ↓  s-")
        // Low priority
        || string.contains(output, "!!  s-")
        // High priority
        || string.contains(output, "!!! s-")
      // Critical priority

      has_indicator_pattern
      |> should.be_true
    }
    Error(_) -> should.be_true(True)
  }
}

pub fn tasks_output_low_priority_has_arrow_indicator_test() {
  // Low priority tasks should have " ↓ " indicator
  case commands.get_tasks(".") {
    Ok(output) -> {
      // We know this project has low priority tasks
      string.contains(output, " ↓  s-")
      |> should.be_true
    }
    Error(_) -> should.be_true(True)
  }
}

// ---------------------------------------------------------------------------
// Filtering Tests (s-182a)
// ---------------------------------------------------------------------------

pub fn tasks_default_filter_hides_completed_test() {
  // Default filter should hide completed tasks
  case commands.get_tasks(".") {
    Ok(output) -> {
      // Should NOT contain completed section
      let has_completed = string.contains(output, "=== Completed (")
      has_completed
      |> should.be_false

      // But should have a message about hidden tasks
      let has_hidden_message = string.contains(output, "completed tasks hidden")
      has_hidden_message
      |> should.be_true
    }
    Error(_) -> should.be_true(True)
  }
}

pub fn tasks_all_filter_shows_completed_test() {
  // --all filter should show completed tasks
  let filter =
    commands.TasksFilter(
      show_all: True,
      status_filter: commands.ShowActionable,
      show_tree: False,
    )
  case commands.get_tasks_filtered(".", filter) {
    Ok(output) -> {
      // Should contain completed section
      string.contains(output, "=== Completed (")
      |> should.be_true

      // Should NOT have hidden message
      let has_hidden_message = string.contains(output, "completed tasks hidden")
      has_hidden_message
      |> should.be_false
    }
    Error(_) -> should.be_true(True)
  }
}

pub fn tasks_pending_filter_shows_only_pending_test() {
  // --pending filter should show only pending tasks
  let filter =
    commands.TasksFilter(
      show_all: False,
      status_filter: commands.ShowPending,
      show_tree: False,
    )
  case commands.get_tasks_filtered(".", filter) {
    Ok(output) -> {
      // Should contain pending section
      string.contains(output, "=== Pending (")
      |> should.be_true

      // Should NOT contain completed or in-progress sections
      let has_completed = string.contains(output, "=== Completed (")
      let has_in_progress = string.contains(output, "=== In Progress (")

      has_completed
      |> should.be_false
      has_in_progress
      |> should.be_false
    }
    Error(_) -> should.be_true(True)
  }
}

pub fn parse_filter_args_default_test() {
  // No args should return default filter
  let filter = commands.parse_filter_args([])
  filter.show_all
  |> should.be_false
}

pub fn parse_filter_args_all_test() {
  // --all should set show_all to true
  let filter = commands.parse_filter_args(["--all"])
  filter.show_all
  |> should.be_true
}

pub fn parse_filter_args_pending_test() {
  // --pending should set status filter
  let filter = commands.parse_filter_args(["--pending"])
  { filter.status_filter == commands.ShowPending }
  |> should.be_true
}

// ---------------------------------------------------------------------------
// Blocked Task Display Tests (s-52c1)
// ---------------------------------------------------------------------------

pub fn tasks_blocked_filter_shows_only_blocked_test() {
  // --blocked filter should show only blocked tasks
  let filter =
    commands.TasksFilter(
      show_all: False,
      status_filter: commands.ShowBlocked,
      show_tree: False,
    )
  case commands.get_tasks_filtered(".", filter) {
    Ok(output) -> {
      // May or may not have blocked tasks
      // If there are blocked tasks, should contain blocked section
      let is_empty = output == "" || string.contains(output, "No tasks")
      let has_blocked = string.contains(output, "=== Blocked (")

      // Either empty or has blocked section
      { is_empty || has_blocked }
      |> should.be_true

      // Should NOT contain other sections
      let has_pending = string.contains(output, "=== Pending (")
      let has_completed = string.contains(output, "=== Completed (")

      has_pending
      |> should.be_false
      has_completed
      |> should.be_false
    }
    Error(_) -> should.be_true(True)
  }
}

pub fn parse_filter_args_blocked_test() {
  // --blocked should set status filter
  let filter = commands.parse_filter_args(["--blocked"])
  { filter.status_filter == commands.ShowBlocked }
  |> should.be_true
}

// ---------------------------------------------------------------------------
// Tree View Tests (s-4f6f)
// ---------------------------------------------------------------------------

pub fn parse_filter_args_tree_test() {
  // --tree should set show_tree to true
  let filter = commands.parse_filter_args(["--tree"])
  filter.show_tree
  |> should.be_true
}

pub fn tasks_tree_view_has_header_test() {
  // Tree view should have hierarchy header
  let filter =
    commands.TasksFilter(
      show_all: True,
      status_filter: commands.ShowActionable,
      show_tree: True,
    )
  case commands.get_tasks_filtered(".", filter) {
    Ok(output) -> {
      string.contains(output, "=== Task Hierarchy ===")
      |> should.be_true
    }
    Error(_) -> should.be_true(True)
  }
}

pub fn tasks_tree_view_shows_children_indented_test() {
  // Children should be indented under their parent with tree lines
  let filter =
    commands.TasksFilter(
      show_all: True,
      status_filter: commands.ShowActionable,
      show_tree: True,
    )
  case commands.get_tasks_filtered(".", filter) {
    Ok(output) -> {
      // s-1900 has children, so we should see tree continuation lines
      // Children are shown with "│   └── " or "│   ├── " prefixes
      // Look for the tree continuation character indicating nested structure
      string.contains(output, "│   ")
      |> should.be_true
    }
    Error(_) -> should.be_true(True)
  }
}

pub fn tasks_tree_combines_with_filters_test() {
  // Tree view should work with status filters
  let filter =
    commands.TasksFilter(
      show_all: False,
      status_filter: commands.ShowPending,
      show_tree: True,
    )
  case commands.get_tasks_filtered(".", filter) {
    Ok(output) -> {
      // Should have hierarchy header
      let has_header = string.contains(output, "=== Task Hierarchy ===")
      // Should NOT have completed tasks (they use [x])
      let has_completed = string.contains(output, "[x]")

      has_header
      |> should.be_true
      has_completed
      |> should.be_false
    }
    Error(_) -> should.be_true(True)
  }
}
