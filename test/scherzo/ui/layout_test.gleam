import gleam/option.{None, Some}
import gleeunit/should
import scherzo/ui/layout
import scherzo/ui/tmux

// ---------------------------------------------------------------------------
// Integration Tests - Layout Management
// These tests require tmux to be available and accessible
// They skip gracefully if tmux isn't working (e.g., in sandboxed environments)
// ---------------------------------------------------------------------------

const test_session = "scherzo-layout-test"

/// Check if tmux is actually usable (not just installed)
fn tmux_is_usable() -> Bool {
  case tmux.create_session("scherzo-layout-availability-test") {
    Ok(_) -> {
      let _ = tmux.kill_session("scherzo-layout-availability-test")
      True
    }
    Error(_) -> False
  }
}

/// Clean up test session
fn cleanup() {
  let _ = tmux.kill_session(test_session)
  Nil
}

pub fn create_layout_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      // Create session first
      tmux.create_session(test_session)
      |> should.be_ok

      // Create layout
      let result = layout.create(test_session)
      case result {
        Ok(l) -> {
          // Should have control pane
          layout.get_control_pane(l)
          |> fn(id) { id != "" }
          |> should.be_true

          // Should have no agents initially
          layout.agent_count(l)
          |> should.equal(0)

          // Agent list pane should be None initially
          l.agent_list_pane
          |> should.equal(None)
        }
        Error(_) -> should.fail()
      }

      cleanup()
    }
  }
}

pub fn create_session_with_layout_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let result = layout.create_session_with_layout(test_session)
      case result {
        Ok(l) -> {
          // Session should exist
          tmux.session_exists(test_session)
          |> should.be_true

          // Should have control pane
          layout.get_control_pane(l)
          |> fn(id) { id != "" }
          |> should.be_true
        }
        Error(_) -> should.fail()
      }

      cleanup()
    }
  }
}

pub fn add_agent_pane_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      // Create layout
      let assert Ok(l) = layout.create_session_with_layout(test_session)

      // Add an agent pane
      let result = layout.add_agent_pane(l, "agent-1")
      case result {
        Ok(l2) -> {
          // Should have one agent
          layout.agent_count(l2)
          |> should.equal(1)

          // Should be able to get agent pane
          layout.get_agent_pane(l2, "agent-1")
          |> fn(opt) {
            case opt {
              Some(_) -> True
              None -> False
            }
          }
          |> should.be_true
        }
        Error(_) -> should.fail()
      }

      cleanup()
    }
  }
}

pub fn add_multiple_agent_panes_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)
      let assert Ok(l) = layout.add_agent_pane(l, "agent-1")
      let assert Ok(l) = layout.add_agent_pane(l, "agent-2")
      let assert Ok(l) = layout.add_agent_pane(l, "agent-3")

      // Should have three agents
      layout.agent_count(l)
      |> should.equal(3)

      // All agents should be listed
      let agents = layout.list_agents(l)
      { agents != [] }
      |> should.be_true

      cleanup()
    }
  }
}

pub fn remove_agent_pane_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)
      let assert Ok(l) = layout.add_agent_pane(l, "agent-1")

      // Remove the agent
      let result = layout.remove_agent_pane(l, "agent-1")
      case result {
        Ok(l2) -> {
          layout.agent_count(l2)
          |> should.equal(0)
        }
        Error(_) -> should.fail()
      }

      cleanup()
    }
  }
}

pub fn remove_nonexistent_agent_fails_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)

      let result = layout.remove_agent_pane(l, "nonexistent")
      case result {
        Error(layout.AgentPaneNotFound(_)) -> should.be_true(True)
        _ -> should.fail()
      }

      cleanup()
    }
  }
}

pub fn focus_control_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)
      let assert Ok(l) = layout.add_agent_pane(l, "agent-1")

      // Focus control should succeed
      layout.focus_control(l)
      |> should.be_ok

      cleanup()
    }
  }
}

pub fn focus_agent_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)
      let assert Ok(l) = layout.add_agent_pane(l, "agent-1")

      // Focus agent should succeed
      layout.focus_agent(l, "agent-1")
      |> should.be_ok

      cleanup()
    }
  }
}

pub fn focus_nonexistent_agent_fails_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)

      let result = layout.focus_agent(l, "nonexistent")
      case result {
        Error(layout.AgentPaneNotFound(_)) -> should.be_true(True)
        _ -> should.fail()
      }

      cleanup()
    }
  }
}

pub fn destroy_layout_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)

      // Destroy should succeed
      layout.destroy(l)
      |> should.be_ok

      // Session should no longer exist
      tmux.session_exists(test_session)
      |> should.be_false
    }
  }
}

pub fn apply_main_layout_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)
      let assert Ok(l) = layout.add_agent_pane(l, "agent-1")

      // Apply main layout should succeed
      layout.apply_main_layout(l)
      |> should.be_ok

      cleanup()
    }
  }
}

pub fn apply_tiled_layout_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)
      let assert Ok(l) = layout.add_agent_pane(l, "agent-1")
      let assert Ok(l) = layout.add_agent_pane(l, "agent-2")

      // Apply tiled layout should succeed
      layout.apply_tiled(l)
      |> should.be_ok

      cleanup()
    }
  }
}

// ---------------------------------------------------------------------------
// Agent List Pane Tests
// ---------------------------------------------------------------------------

// Use a simple command that doesn't exit immediately for testing
// (scherzo agent-list doesn't exist yet)
const test_agent_list_command = "sleep 300"

pub fn create_agent_list_pane_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)

      // Initially no agent list pane
      layout.has_agent_list_pane(l)
      |> should.be_false

      // Create agent list pane
      let result =
        layout.create_agent_list_pane_with_command(l, 5, test_agent_list_command)
      case result {
        Ok(l2) -> {
          // Should have agent list pane now
          layout.has_agent_list_pane(l2)
          |> should.be_true

          // Pane ID should exist
          layout.get_agent_list_pane(l2)
          |> option.is_some
          |> should.be_true
        }
        Error(_) -> should.fail()
      }

      cleanup()
    }
  }
}

pub fn create_agent_list_pane_already_exists_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)
      let assert Ok(l) =
        layout.create_agent_list_pane_with_command(l, 5, test_agent_list_command)

      // Try to create again - should fail
      let result =
        layout.create_agent_list_pane_with_command(l, 5, test_agent_list_command)
      case result {
        Error(layout.AgentListPaneExists) -> should.be_true(True)
        _ -> should.fail()
      }

      cleanup()
    }
  }
}

pub fn destroy_agent_list_pane_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)
      let assert Ok(l) =
        layout.create_agent_list_pane_with_command(l, 5, test_agent_list_command)

      // Destroy the pane
      let result = layout.destroy_agent_list_pane(l)
      case result {
        Ok(l2) -> {
          // Should not have agent list pane
          layout.has_agent_list_pane(l2)
          |> should.be_false
        }
        Error(_) -> should.fail()
      }

      cleanup()
    }
  }
}

pub fn destroy_agent_list_pane_not_found_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)

      // Try to destroy when not exists - should fail
      let result = layout.destroy_agent_list_pane(l)
      case result {
        Error(layout.AgentListPaneNotFound) -> should.be_true(True)
        _ -> should.fail()
      }

      cleanup()
    }
  }
}

pub fn toggle_agent_list_pane_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      cleanup()

      let assert Ok(l) = layout.create_session_with_layout(test_session)

      // Initially no pane
      layout.has_agent_list_pane(l)
      |> should.be_false

      // Toggle on
      let assert Ok(l) =
        layout.toggle_agent_list_pane_with_command(l, 5, test_agent_list_command)
      layout.has_agent_list_pane(l)
      |> should.be_true

      // Toggle off
      let assert Ok(l) =
        layout.toggle_agent_list_pane_with_command(l, 5, test_agent_list_command)
      layout.has_agent_list_pane(l)
      |> should.be_false

      cleanup()
    }
  }
}
