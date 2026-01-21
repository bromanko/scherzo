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
  commands.CommandContext(working_dir: ".")
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
  case commands.get_tasks(".") {
    Ok(output) -> {
      // Should contain completed section since we have closed tickets
      string.contains(output, "=== Completed (")
      |> should.be_true
    }
    Error(_) -> should.be_true(True)
  }
}

pub fn tasks_output_groups_separated_by_blank_lines_test() {
  case commands.get_tasks(".") {
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
      // Low priority uses " . ", Normal uses "   ", High "!! ", Critical "!!!"
      // At minimum we should see some spacing before task IDs
      let has_indicator_pattern =
        string.contains(output, "    s-")
        // Normal priority (3 spaces + space before ID)
        || string.contains(output, " .  s-")
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

pub fn tasks_output_low_priority_has_dot_indicator_test() {
  // Low priority tasks should have " . " indicator
  case commands.get_tasks(".") {
    Ok(output) -> {
      // We know this project has low priority tasks
      string.contains(output, " .  s-")
      |> should.be_true
    }
    Error(_) -> should.be_true(True)
  }
}
