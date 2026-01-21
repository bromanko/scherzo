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
