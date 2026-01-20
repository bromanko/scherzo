import gleam/option.{None, Some}
import gleeunit/should
import scherzo/ui/pipes
import scherzo/ui/session_manager
import scherzo/ui/tmux
import shellout
import simplifile

// ---------------------------------------------------------------------------
// Test Helpers
// ---------------------------------------------------------------------------

const test_session = "scherzo-session-test"

const test_working_dir = "/tmp/claude/scherzo-session-test"

fn cleanup() {
  let _ = tmux.kill_session(test_session)
  let _ = simplifile.delete_all([test_working_dir])
  Nil
}

/// Check if tmux and mkfifo are available
fn prereqs_available() -> Bool {
  // Check tmux
  let tmux_ok = case tmux.create_session("scherzo-prereq-test") {
    Ok(_) -> {
      let _ = tmux.kill_session("scherzo-prereq-test")
      True
    }
    Error(_) -> False
  }

  // Check mkfifo
  let mkfifo_ok = {
    let test_path = "/tmp/claude/scherzo-mkfifo-prereq"
    let _ = simplifile.create_directory_all("/tmp/claude")
    let _ = simplifile.delete(test_path)
    case shellout.command(run: "mkfifo", with: [test_path], in: ".", opt: []) {
      Ok(_) -> {
        let _ = simplifile.delete(test_path)
        True
      }
      Error(_) -> False
    }
  }

  tmux_ok && mkfifo_ok
}

// ---------------------------------------------------------------------------
// Session Creation Tests
// ---------------------------------------------------------------------------

pub fn create_session_test() {
  case prereqs_available() {
    False -> Nil
    True -> {
      cleanup()
      let _ = simplifile.create_directory_all(test_working_dir)

      let result = session_manager.create(test_session, test_working_dir)
      case result {
        Ok(manager) -> {
          // Session should exist
          tmux.session_exists(test_session)
          |> should.be_true

          // Should have no agents initially
          session_manager.agent_count(manager)
          |> should.equal(0)

          // Clean up
          let _ = session_manager.destroy(manager)
          Nil
        }
        Error(_) -> {
          should.fail()
          Nil
        }
      }

      cleanup()
    }
  }
}

// ---------------------------------------------------------------------------
// Agent Management Tests
// ---------------------------------------------------------------------------

pub fn add_agent_creates_pane_and_pipe_test() {
  case prereqs_available() {
    False -> Nil
    True -> {
      cleanup()
      let _ = simplifile.create_directory_all(test_working_dir)

      let assert Ok(manager) =
        session_manager.create(test_session, test_working_dir)

      // Add an agent
      let result = session_manager.add_agent(manager, "agent-1")
      case result {
        Ok(#(new_manager, pipe_path)) -> {
          // Should have one agent
          session_manager.agent_count(new_manager)
          |> should.equal(1)

          // Pipe should exist (check with test -p)
          let pipe_config = pipes.default_config(test_working_dir)
          pipes.pipe_exists(pipe_config, "agent-1")
          |> should.be_true

          // Pipe path should be returned
          { pipe_path != "" }
          |> should.be_true

          // Clean up
          let _ = session_manager.destroy(new_manager)
          Nil
        }
        Error(_) -> {
          should.fail()
          Nil
        }
      }

      cleanup()
    }
  }
}

pub fn add_multiple_agents_test() {
  case prereqs_available() {
    False -> Nil
    True -> {
      cleanup()
      let _ = simplifile.create_directory_all(test_working_dir)

      let assert Ok(manager) =
        session_manager.create(test_session, test_working_dir)
      let assert Ok(#(manager, _)) =
        session_manager.add_agent(manager, "agent-1")
      let assert Ok(#(manager, _)) =
        session_manager.add_agent(manager, "agent-2")
      let assert Ok(#(manager, _)) =
        session_manager.add_agent(manager, "agent-3")

      // Should have three agents
      session_manager.agent_count(manager)
      |> should.equal(3)

      // All agents should be listed
      let agents = session_manager.list_agents(manager)
      { agents != [] }
      |> should.be_true

      // Clean up
      let _ = session_manager.destroy(manager)
      cleanup()
    }
  }
}

pub fn remove_agent_cleans_up_test() {
  case prereqs_available() {
    False -> Nil
    True -> {
      cleanup()
      let _ = simplifile.create_directory_all(test_working_dir)

      let assert Ok(manager) =
        session_manager.create(test_session, test_working_dir)
      let assert Ok(#(manager, _)) =
        session_manager.add_agent(manager, "agent-1")

      // Remove the agent
      let result = session_manager.remove_agent(manager, "agent-1")
      case result {
        Ok(new_manager) -> {
          // Should have no agents
          session_manager.agent_count(new_manager)
          |> should.equal(0)

          // Pipe should be gone
          let pipe_config = pipes.default_config(test_working_dir)
          pipes.pipe_exists(pipe_config, "agent-1")
          |> should.be_false

          // Clean up
          let _ = session_manager.destroy(new_manager)
          Nil
        }
        Error(_) -> {
          should.fail()
          Nil
        }
      }

      cleanup()
    }
  }
}

pub fn get_agent_pipe_test() {
  case prereqs_available() {
    False -> Nil
    True -> {
      cleanup()
      let _ = simplifile.create_directory_all(test_working_dir)

      let assert Ok(manager) =
        session_manager.create(test_session, test_working_dir)
      let assert Ok(#(manager, _)) =
        session_manager.add_agent(manager, "agent-1")

      // Should get pipe path for existing agent
      case session_manager.get_agent_pipe(manager, "agent-1") {
        Some(path) -> {
          { path != "" }
          |> should.be_true
        }
        None -> should.fail()
      }

      // Should not get pipe for non-existent agent
      session_manager.get_agent_pipe(manager, "nonexistent")
      |> should.equal(None)

      // Clean up
      let _ = session_manager.destroy(manager)
      cleanup()
    }
  }
}

// ---------------------------------------------------------------------------
// Focus Tests
// ---------------------------------------------------------------------------

pub fn focus_control_test() {
  case prereqs_available() {
    False -> Nil
    True -> {
      cleanup()
      let _ = simplifile.create_directory_all(test_working_dir)

      let assert Ok(manager) =
        session_manager.create(test_session, test_working_dir)
      let assert Ok(#(manager, _)) =
        session_manager.add_agent(manager, "agent-1")

      // Focus control should succeed
      session_manager.focus_control(manager)
      |> should.be_ok

      // Clean up
      let _ = session_manager.destroy(manager)
      cleanup()
    }
  }
}

pub fn focus_agent_test() {
  case prereqs_available() {
    False -> Nil
    True -> {
      cleanup()
      let _ = simplifile.create_directory_all(test_working_dir)

      let assert Ok(manager) =
        session_manager.create(test_session, test_working_dir)
      let assert Ok(#(manager, _)) =
        session_manager.add_agent(manager, "agent-1")

      // Focus agent should succeed
      session_manager.focus_agent(manager, "agent-1")
      |> should.be_ok

      // Focus nonexistent agent should fail
      session_manager.focus_agent(manager, "nonexistent")
      |> should.be_error

      // Clean up
      let _ = session_manager.destroy(manager)
      cleanup()
    }
  }
}

// ---------------------------------------------------------------------------
// Destroy Tests
// ---------------------------------------------------------------------------

pub fn destroy_cleans_up_everything_test() {
  case prereqs_available() {
    False -> Nil
    True -> {
      cleanup()
      let _ = simplifile.create_directory_all(test_working_dir)

      let assert Ok(manager) =
        session_manager.create(test_session, test_working_dir)
      let assert Ok(#(manager, _)) =
        session_manager.add_agent(manager, "agent-1")
      let assert Ok(#(manager, _)) =
        session_manager.add_agent(manager, "agent-2")

      // Destroy
      session_manager.destroy(manager)
      |> should.be_ok

      // Session should not exist
      tmux.session_exists(test_session)
      |> should.be_false

      cleanup()
    }
  }
}

// ---------------------------------------------------------------------------
// Utility Tests
// ---------------------------------------------------------------------------

pub fn session_name_test() {
  case prereqs_available() {
    False -> Nil
    True -> {
      cleanup()
      let _ = simplifile.create_directory_all(test_working_dir)

      let assert Ok(manager) =
        session_manager.create(test_session, test_working_dir)

      session_manager.session_name(manager)
      |> should.equal(test_session)

      // Clean up
      let _ = session_manager.destroy(manager)
      cleanup()
    }
  }
}

pub fn control_pane_id_test() {
  case prereqs_available() {
    False -> Nil
    True -> {
      cleanup()
      let _ = simplifile.create_directory_all(test_working_dir)

      let assert Ok(manager) =
        session_manager.create(test_session, test_working_dir)

      // Control pane ID should be set
      let pane_id = session_manager.control_pane_id(manager)
      { pane_id != "" }
      |> should.be_true

      // Clean up
      let _ = session_manager.destroy(manager)
      cleanup()
    }
  }
}
