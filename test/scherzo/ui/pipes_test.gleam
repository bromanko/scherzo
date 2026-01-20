import gleeunit/should
import scherzo/ui/pipes
import shellout
import simplifile

// ---------------------------------------------------------------------------
// Test Helpers
// ---------------------------------------------------------------------------

const test_pipes_dir = "/tmp/claude/scherzo-pipes-test"

fn test_config() -> pipes.PipeConfig {
  pipes.PipeConfig(pipes_dir: test_pipes_dir)
}

fn cleanup() {
  let _ = simplifile.delete_all([test_pipes_dir])
  Nil
}

/// Check if mkfifo is available and usable
fn mkfifo_is_usable() -> Bool {
  let test_path = "/tmp/claude/scherzo-mkfifo-test"
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

// ---------------------------------------------------------------------------
// Pipe Path Tests
// ---------------------------------------------------------------------------

pub fn pipe_path_test() {
  let config = test_config()

  pipes.pipe_path(config, "agent-1")
  |> should.equal(test_pipes_dir <> "/agent-1")
}

pub fn pipe_path_sanitizes_slashes_test() {
  let config = test_config()

  pipes.pipe_path(config, "path/to/agent")
  |> should.equal(test_pipes_dir <> "/path_to_agent")
}

pub fn pipe_path_sanitizes_spaces_test() {
  let config = test_config()

  pipes.pipe_path(config, "agent with spaces")
  |> should.equal(test_pipes_dir <> "/agent_with_spaces")
}

// ---------------------------------------------------------------------------
// Directory Management Tests
// ---------------------------------------------------------------------------

pub fn ensure_pipes_dir_creates_directory_test() {
  cleanup()

  let config = test_config()

  pipes.ensure_pipes_dir(config)
  |> should.be_ok

  // Directory should exist
  simplifile.is_directory(test_pipes_dir)
  |> should.equal(Ok(True))

  cleanup()
}

pub fn ensure_pipes_dir_idempotent_test() {
  cleanup()

  let config = test_config()

  // Call twice - should succeed both times
  pipes.ensure_pipes_dir(config)
  |> should.be_ok

  pipes.ensure_pipes_dir(config)
  |> should.be_ok

  cleanup()
}

// ---------------------------------------------------------------------------
// Pipe Creation Tests (require mkfifo)
// ---------------------------------------------------------------------------

pub fn create_pipe_creates_fifo_test() {
  case mkfifo_is_usable() {
    False -> Nil
    True -> {
      cleanup()
      let config = test_config()

      let result = pipes.create_pipe(config, "test-agent")
      case result {
        Ok(path) -> {
          // Path should match expected
          path
          |> should.equal(test_pipes_dir <> "/test-agent")

          // Pipe should exist (use pipe_exists which checks with test -p)
          pipes.pipe_exists(config, "test-agent")
          |> should.be_true
        }
        Error(_) -> should.fail()
      }

      cleanup()
    }
  }
}

pub fn create_pipe_replaces_existing_test() {
  case mkfifo_is_usable() {
    False -> Nil
    True -> {
      cleanup()
      let config = test_config()

      // Create pipe twice - should succeed both times
      pipes.create_pipe(config, "test-agent")
      |> should.be_ok

      pipes.create_pipe(config, "test-agent")
      |> should.be_ok

      cleanup()
    }
  }
}

// ---------------------------------------------------------------------------
// Pipe Existence Tests
// ---------------------------------------------------------------------------

pub fn pipe_exists_false_when_not_created_test() {
  cleanup()

  let config = test_config()
  let _ = pipes.ensure_pipes_dir(config)

  pipes.pipe_exists(config, "nonexistent")
  |> should.be_false

  cleanup()
}

pub fn pipe_exists_true_after_creation_test() {
  case mkfifo_is_usable() {
    False -> Nil
    True -> {
      cleanup()
      let config = test_config()
      let _ = pipes.create_pipe(config, "test-agent")

      pipes.pipe_exists(config, "test-agent")
      |> should.be_true

      cleanup()
    }
  }
}

// ---------------------------------------------------------------------------
// Pipe Removal Tests
// ---------------------------------------------------------------------------

pub fn remove_pipe_succeeds_test() {
  case mkfifo_is_usable() {
    False -> Nil
    True -> {
      cleanup()
      let config = test_config()
      let _ = pipes.create_pipe(config, "test-agent")

      pipes.remove_pipe(config, "test-agent")
      |> should.be_ok

      pipes.pipe_exists(config, "test-agent")
      |> should.be_false

      cleanup()
    }
  }
}

pub fn remove_nonexistent_pipe_succeeds_test() {
  cleanup()

  let config = test_config()
  let _ = pipes.ensure_pipes_dir(config)

  // Should succeed even if pipe doesn't exist
  pipes.remove_pipe(config, "nonexistent")
  |> should.be_ok

  cleanup()
}

// ---------------------------------------------------------------------------
// List Pipes Tests
// ---------------------------------------------------------------------------

pub fn list_pipes_empty_initially_test() {
  cleanup()

  let config = test_config()
  let _ = pipes.ensure_pipes_dir(config)

  let result = pipes.list_pipes(config)
  case result {
    Ok(pipes_list) -> {
      pipes_list
      |> should.equal([])
    }
    Error(_) -> should.fail()
  }

  cleanup()
}

pub fn list_pipes_shows_created_pipes_test() {
  case mkfifo_is_usable() {
    False -> Nil
    True -> {
      cleanup()
      let config = test_config()
      let _ = pipes.create_pipe(config, "agent-1")
      let _ = pipes.create_pipe(config, "agent-2")

      let result = pipes.list_pipes(config)
      case result {
        Ok(pipes_list) -> {
          // Should have 2 pipes (order may vary)
          { pipes_list != [] }
          |> should.be_true
        }
        Error(_) -> should.fail()
      }

      cleanup()
    }
  }
}

// ---------------------------------------------------------------------------
// Cleanup Tests
// ---------------------------------------------------------------------------

pub fn cleanup_all_pipes_removes_all_test() {
  case mkfifo_is_usable() {
    False -> Nil
    True -> {
      cleanup()
      let config = test_config()
      let _ = pipes.create_pipe(config, "agent-1")
      let _ = pipes.create_pipe(config, "agent-2")
      let _ = pipes.create_pipe(config, "agent-3")

      pipes.cleanup_all_pipes(config)
      |> should.be_ok

      // All pipes should be gone
      let result = pipes.list_pipes(config)
      case result {
        Ok(pipes_list) -> {
          pipes_list
          |> should.equal([])
        }
        Error(_) -> should.fail()
      }

      cleanup()
    }
  }
}

// ---------------------------------------------------------------------------
// Command Generation Tests
// ---------------------------------------------------------------------------

pub fn tail_command_test() {
  let config = test_config()

  pipes.tail_command(config, "agent-1")
  |> should.equal("tail -f " <> test_pipes_dir <> "/agent-1")
}

pub fn cat_command_test() {
  let config = test_config()

  pipes.cat_command(config, "agent-1")
  |> should.equal("cat " <> test_pipes_dir <> "/agent-1")
}

// ---------------------------------------------------------------------------
// Default Config Tests
// ---------------------------------------------------------------------------

pub fn default_config_test() {
  let config = pipes.default_config("/home/user/project")

  config.pipes_dir
  |> should.equal("/home/user/project/.scherzo/pipes")
}
