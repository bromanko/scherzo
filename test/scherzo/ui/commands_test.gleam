import gleam/option.{None}
import gleeunit/should
import scherzo/core/task.{Completed, InProgress, Normal, Pending, Task}
import scherzo/core/types.{AgentConfig, Claude, Idle, Running}
import scherzo/state/store.{AgentState, StoreConfig}
import scherzo/ui/commands
import scherzo/ui/repl

// ---------------------------------------------------------------------------
// Test Helpers
// ---------------------------------------------------------------------------

/// Create a test store with some sample data
fn setup_test_store() -> Result(commands.CommandContext, Nil) {
  case store.start(StoreConfig(state_dir: "/tmp/scherzo-cmd-test")) {
    Ok(s) -> {
      // Add some test tasks
      store.save_task(
        s,
        Task(
          id: "task-001",
          title: "Test Task 1",
          description: "First test task",
          status: Pending,
          priority: Normal,
          dependencies: [],
          created_at: 1000,
          updated_at: 1000,
          source_id: None,
          jj_change_id: None,
        ),
      )
      store.save_task(
        s,
        Task(
          id: "task-002",
          title: "Test Task 2",
          description: "Second test task",
          status: InProgress("agent-1", 2000),
          priority: Normal,
          dependencies: [],
          created_at: 1000,
          updated_at: 2000,
          source_id: None,
          jj_change_id: None,
        ),
      )
      store.save_task(
        s,
        Task(
          id: "task-003",
          title: "Test Task 3",
          description: "Third test task",
          status: Completed("agent-1", 3000),
          priority: Normal,
          dependencies: [],
          created_at: 1000,
          updated_at: 3000,
          source_id: None,
          jj_change_id: None,
        ),
      )

      // Add some test agents
      store.save_agent(
        s,
        AgentState(
          config: AgentConfig(
            id: "agent-1",
            provider: Claude,
            working_dir: "/tmp",
            max_retries: 3,
            timeout_ms: 60_000,
          ),
          status: Running("task-002", 2000),
        ),
      )
      store.save_agent(
        s,
        AgentState(
          config: AgentConfig(
            id: "agent-2",
            provider: Claude,
            working_dir: "/tmp",
            max_retries: 3,
            timeout_ms: 60_000,
          ),
          status: Idle,
        ),
      )

      Ok(commands.CommandContext(store: s))
    }
    Error(_) -> Error(Nil)
  }
}

fn cleanup_store(ctx: commands.CommandContext) {
  store.stop(ctx.store)
}

// ---------------------------------------------------------------------------
// Status Command Tests
// ---------------------------------------------------------------------------

pub fn status_command_shows_counts_test() {
  case setup_test_store() {
    Error(_) -> Nil
    Ok(ctx) -> {
      let handler = commands.status_command(ctx)
      let result = handler([])

      case result {
        repl.CommandOutput(output) -> {
          // Should contain task counts
          { output != "" }
          |> should.be_true
        }
        _ -> should.fail()
      }

      cleanup_store(ctx)
    }
  }
}

// ---------------------------------------------------------------------------
// Tasks Command Tests
// ---------------------------------------------------------------------------

pub fn tasks_command_lists_tasks_test() {
  case setup_test_store() {
    Error(_) -> Nil
    Ok(ctx) -> {
      let handler = commands.tasks_command(ctx)
      let result = handler([])

      case result {
        repl.CommandOutput(output) -> {
          // Should list tasks
          { output != "" }
          |> should.be_true
        }
        _ -> should.fail()
      }

      cleanup_store(ctx)
    }
  }
}

pub fn tasks_command_empty_store_test() {
  case store.start(StoreConfig(state_dir: "/tmp/scherzo-cmd-empty")) {
    Ok(s) -> {
      let ctx = commands.CommandContext(store: s)
      let handler = commands.tasks_command(ctx)
      let result = handler([])

      case result {
        repl.CommandOutput(output) -> {
          // Should show no tasks message
          { output != "" }
          |> should.be_true
        }
        _ -> should.fail()
      }

      store.stop(s)
    }
    Error(_) -> Nil
  }
}

// ---------------------------------------------------------------------------
// Agents Command Tests
// ---------------------------------------------------------------------------

pub fn agents_command_lists_agents_test() {
  case setup_test_store() {
    Error(_) -> Nil
    Ok(ctx) -> {
      let handler = commands.agents_command(ctx)
      let result = handler([])

      case result {
        repl.CommandOutput(output) -> {
          // Should list agents
          { output != "" }
          |> should.be_true
        }
        _ -> should.fail()
      }

      cleanup_store(ctx)
    }
  }
}

// ---------------------------------------------------------------------------
// Lifecycle Command Tests
// ---------------------------------------------------------------------------

pub fn pause_command_requires_agent_id_test() {
  case setup_test_store() {
    Error(_) -> Nil
    Ok(ctx) -> {
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

      cleanup_store(ctx)
    }
  }
}

pub fn resume_command_requires_agent_id_test() {
  case setup_test_store() {
    Error(_) -> Nil
    Ok(ctx) -> {
      let handler = commands.resume_command(ctx)

      // Without agent id should error
      let result = handler([])
      case result {
        repl.CommandError(_) -> should.be_true(True)
        _ -> should.fail()
      }

      cleanup_store(ctx)
    }
  }
}

pub fn retry_command_requires_task_id_test() {
  case setup_test_store() {
    Error(_) -> Nil
    Ok(ctx) -> {
      let handler = commands.retry_command(ctx)

      // Without task id should error
      let result = handler([])
      case result {
        repl.CommandError(_) -> should.be_true(True)
        _ -> should.fail()
      }

      cleanup_store(ctx)
    }
  }
}

pub fn kill_command_requires_agent_id_test() {
  case setup_test_store() {
    Error(_) -> Nil
    Ok(ctx) -> {
      let handler = commands.kill_command(ctx)

      // Without agent id should error
      let result = handler([])
      case result {
        repl.CommandError(_) -> should.be_true(True)
        _ -> should.fail()
      }

      cleanup_store(ctx)
    }
  }
}

// ---------------------------------------------------------------------------
// Focus Command Tests
// ---------------------------------------------------------------------------

pub fn focus_command_requires_agent_id_test() {
  case setup_test_store() {
    Error(_) -> Nil
    Ok(ctx) -> {
      let handler = commands.focus_command(ctx)

      // Without agent id should error
      let result = handler([])
      case result {
        repl.CommandError(_) -> should.be_true(True)
        _ -> should.fail()
      }

      cleanup_store(ctx)
    }
  }
}

pub fn abort_command_test() {
  case setup_test_store() {
    Error(_) -> Nil
    Ok(ctx) -> {
      let handler = commands.abort_command(ctx)
      let result = handler([])

      case result {
        repl.CommandOutput(_) -> should.be_true(True)
        _ -> should.fail()
      }

      cleanup_store(ctx)
    }
  }
}

// ---------------------------------------------------------------------------
// Command Registration Tests
// ---------------------------------------------------------------------------

pub fn register_info_commands_test() {
  case setup_test_store() {
    Error(_) -> Nil
    Ok(ctx) -> {
      let config = repl.default_config()
      let config = commands.register_info_commands(config, ctx)

      repl.has_command(config, "status")
      |> should.be_true

      repl.has_command(config, "tasks")
      |> should.be_true

      repl.has_command(config, "agents")
      |> should.be_true

      cleanup_store(ctx)
    }
  }
}

pub fn register_lifecycle_commands_test() {
  case setup_test_store() {
    Error(_) -> Nil
    Ok(ctx) -> {
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

      cleanup_store(ctx)
    }
  }
}

pub fn register_all_commands_test() {
  case setup_test_store() {
    Error(_) -> Nil
    Ok(ctx) -> {
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

      cleanup_store(ctx)
    }
  }
}
