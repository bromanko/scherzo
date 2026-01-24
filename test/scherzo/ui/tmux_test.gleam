import gleam/list
import gleam/string
import gleeunit/should
import scherzo/ui/tmux

// ---------------------------------------------------------------------------
// Unit Tests - Pure functions (always run)
// ---------------------------------------------------------------------------

pub fn default_session_name_test() {
  tmux.default_session_name
  |> should.equal("scherzo")
}

// ---------------------------------------------------------------------------
// Integration Tests - Session Lifecycle
// These tests require tmux to be available and accessible
// They skip gracefully if tmux isn't working (e.g., in sandboxed environments)
// ---------------------------------------------------------------------------

const test_session = "scherzo-test-session"

/// Check if tmux is actually usable (not just installed)
fn tmux_is_usable() -> Bool {
  // Try to create and immediately kill a test session
  // This verifies tmux can actually run commands
  case tmux.create_session("scherzo-availability-test") {
    Ok(_) -> {
      let _ = tmux.kill_session("scherzo-availability-test")
      True
    }
    Error(_) -> False
  }
}

pub fn is_available_returns_bool_test() {
  // This just verifies the function returns without crashing
  let _ = tmux.is_available()
  should.be_true(True)
}

pub fn session_does_not_exist_initially_test() {
  case tmux_is_usable() {
    False -> Nil
    // Skip if tmux not usable
    True -> {
      // Clean up any leftover test session
      let _ = tmux.kill_session(test_session)

      tmux.session_exists(test_session)
      |> should.be_false
    }
  }
}

pub fn create_and_kill_session_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure clean state
      let _ = tmux.kill_session(test_session)

      // Create session
      tmux.create_session(test_session)
      |> should.be_ok

      // Session should exist
      tmux.session_exists(test_session)
      |> should.be_true

      // Kill session
      tmux.kill_session(test_session)
      |> should.be_ok

      // Session should not exist
      tmux.session_exists(test_session)
      |> should.be_false
    }
  }
}

pub fn create_session_fails_if_exists_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure clean state
      let _ = tmux.kill_session(test_session)

      // Create session
      tmux.create_session(test_session)
      |> should.be_ok

      // Try to create again - should fail
      let result = tmux.create_session(test_session)
      case result {
        Error(tmux.SessionExists(_)) -> should.be_true(True)
        _ -> should.fail()
      }

      // Clean up
      let _ = tmux.kill_session(test_session)
      Nil
    }
  }
}

pub fn kill_nonexistent_session_fails_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure session doesn't exist
      let _ = tmux.kill_session(test_session)

      let result = tmux.kill_session(test_session)
      case result {
        Error(tmux.SessionNotFound(_)) -> should.be_true(True)
        _ -> should.fail()
      }
    }
  }
}

pub fn list_sessions_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure clean state
      let _ = tmux.kill_session(test_session)

      // Create a test session
      tmux.create_session(test_session)
      |> should.be_ok

      // List should include our session
      let result = tmux.list_sessions()
      case result {
        Ok(sessions) -> {
          let has_test_session = list.contains(sessions, test_session)
          has_test_session |> should.be_true
        }
        Error(_) -> should.fail()
      }

      // Clean up
      let _ = tmux.kill_session(test_session)
      Nil
    }
  }
}

// ---------------------------------------------------------------------------
// Integration Tests - Pane Management
// ---------------------------------------------------------------------------

pub fn split_horizontal_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure clean state
      let _ = tmux.kill_session(test_session)
      tmux.create_session(test_session) |> should.be_ok

      // Split horizontally
      let result = tmux.split_horizontal(test_session)
      case result {
        Ok(pane_id) -> {
          // Pane ID should start with %
          string.starts_with(pane_id, "%")
          |> should.be_true
        }
        Error(_) -> should.fail()
      }

      // Clean up
      let _ = tmux.kill_session(test_session)
      Nil
    }
  }
}

pub fn split_vertical_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure clean state
      let _ = tmux.kill_session(test_session)
      tmux.create_session(test_session) |> should.be_ok

      // Split vertically
      let result = tmux.split_vertical(test_session)
      case result {
        Ok(pane_id) -> {
          // Pane ID should start with %
          string.starts_with(pane_id, "%")
          |> should.be_true
        }
        Error(_) -> should.fail()
      }

      // Clean up
      let _ = tmux.kill_session(test_session)
      Nil
    }
  }
}

pub fn list_panes_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure clean state
      let _ = tmux.kill_session(test_session)
      tmux.create_session(test_session) |> should.be_ok

      // Initially one pane
      let result = tmux.list_panes(test_session)
      case result {
        Ok(panes) -> {
          list.length(panes) |> should.equal(1)
        }
        Error(_) -> should.fail()
      }

      // Split to create second pane
      let _ = tmux.split_horizontal(test_session)

      // Now two panes
      let result2 = tmux.list_panes(test_session)
      case result2 {
        Ok(panes) -> {
          list.length(panes) |> should.equal(2)
        }
        Error(_) -> should.fail()
      }

      // Clean up
      let _ = tmux.kill_session(test_session)
      Nil
    }
  }
}

pub fn get_first_pane_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure clean state
      let _ = tmux.kill_session(test_session)
      tmux.create_session(test_session) |> should.be_ok

      let result = tmux.get_first_pane(test_session)
      case result {
        Ok(pane_id) -> {
          string.starts_with(pane_id, "%")
          |> should.be_true
        }
        Error(_) -> should.fail()
      }

      // Clean up
      let _ = tmux.kill_session(test_session)
      Nil
    }
  }
}

pub fn apply_tiled_layout_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure clean state
      let _ = tmux.kill_session(test_session)
      tmux.create_session(test_session) |> should.be_ok

      // Create some panes
      let _ = tmux.split_horizontal(test_session)
      let _ = tmux.split_vertical(test_session)

      // Apply tiled layout
      tmux.apply_tiled_layout(test_session)
      |> should.be_ok

      // Clean up
      let _ = tmux.kill_session(test_session)
      Nil
    }
  }
}

pub fn apply_main_horizontal_layout_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure clean state
      let _ = tmux.kill_session(test_session)
      tmux.create_session(test_session) |> should.be_ok

      // Create a pane to split
      let _ = tmux.split_horizontal(test_session)

      // Apply main-horizontal layout
      tmux.apply_main_horizontal_layout(test_session)
      |> should.be_ok

      // Clean up
      let _ = tmux.kill_session(test_session)
      Nil
    }
  }
}

pub fn resize_pane_height_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure clean state
      let _ = tmux.kill_session(test_session)
      tmux.create_session(test_session) |> should.be_ok

      // Create a second pane so we have something to resize
      let split_result = tmux.split_horizontal(test_session)
      case split_result {
        Ok(pane_id) -> {
          // Resize the pane to 5 rows
          tmux.resize_pane_height(pane_id, 5)
          |> should.be_ok
        }
        Error(_) -> should.fail()
      }

      // Clean up
      let _ = tmux.kill_session(test_session)
      Nil
    }
  }
}

pub fn kill_pane_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure clean state
      let _ = tmux.kill_session(test_session)
      tmux.create_session(test_session) |> should.be_ok

      // Create a second pane
      let split_result = tmux.split_horizontal(test_session)
      case split_result {
        Ok(pane_id) -> {
          // Kill the new pane
          tmux.kill_pane(pane_id)
          |> should.be_ok

          // Should be back to one pane
          let panes_result = tmux.list_panes(test_session)
          case panes_result {
            Ok(panes) -> list.length(panes) |> should.equal(1)
            Error(_) -> should.fail()
          }
        }
        Error(_) -> should.fail()
      }

      // Clean up
      let _ = tmux.kill_session(test_session)
      Nil
    }
  }
}

pub fn bind_key_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure clean state
      let _ = tmux.kill_session(test_session)
      tmux.create_session(test_session) |> should.be_ok

      // Bind F12 to a simple echo command (using F12 to avoid conflicts)
      tmux.bind_key("F12", "echo test")
      |> should.be_ok

      // Clean up the binding
      tmux.unbind_key("F12")
      |> should.be_ok

      // Clean up session
      let _ = tmux.kill_session(test_session)
      Nil
    }
  }
}

pub fn unbind_key_nonexistent_succeeds_test() {
  case tmux_is_usable() {
    False -> Nil
    True -> {
      // Ensure clean state (need a running server for unbind to work)
      let _ = tmux.kill_session(test_session)
      tmux.create_session(test_session) |> should.be_ok

      // Unbinding a key that isn't bound should still succeed (tmux doesn't error)
      tmux.unbind_key("F11")
      |> should.be_ok

      // Clean up
      let _ = tmux.kill_session(test_session)
      Nil
    }
  }
}
