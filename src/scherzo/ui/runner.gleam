/// UI Runner - wires together REPL, commands, and orchestrator
///
/// Provides the startup sequence for the scherzo tmux UI,
/// initializing all actors and running the control REPL.
import gleam/io
import scherzo/state/store
import scherzo/ui/commands
import scherzo/ui/repl
import scherzo/ui/session_manager

/// Configuration for the UI runner
pub type RunnerConfig {
  RunnerConfig(
    /// Session name for tmux
    session_name: String,
    /// Working directory
    working_dir: String,
    /// State directory for persistence
    state_dir: String,
  )
}

/// Default runner config
pub fn default_config(working_dir: String) -> RunnerConfig {
  RunnerConfig(
    session_name: "scherzo",
    working_dir: working_dir,
    state_dir: working_dir <> "/.scherzo/state",
  )
}

/// Error during UI startup
pub type RunnerError {
  /// Failed to start state store
  StoreError(String)
  /// Failed to create tmux session
  SessionError(String)
}

/// Start the UI with all components wired together
/// This function blocks until the REPL exits
pub fn start(config: RunnerConfig) -> Result(Nil, RunnerError) {
  // Start state store
  case store.start(store.StoreConfig(state_dir: config.state_dir)) {
    Error(_) -> Error(StoreError("Failed to start state store"))
    Ok(store_subject) -> {
      // Load persisted state
      store.load(store_subject)

      // Create command context
      let command_ctx = commands.CommandContext(store: store_subject)

      // Create REPL config with all commands
      let repl_config =
        repl.default_config()
        |> commands.register_all_commands(command_ctx)

      // Print startup message
      io.println("Scherzo Control REPL")
      io.println("Type 'help' for available commands")
      io.println("")

      // Run the REPL (blocks until quit)
      repl.run(repl_config)

      // Persist state before shutdown
      store.persist(store_subject)
      store.stop(store_subject)

      Ok(Nil)
    }
  }
}

/// Start the UI with a tmux session
/// Creates session, initializes REPL, and runs
pub fn start_with_session(config: RunnerConfig) -> Result(Nil, RunnerError) {
  // Create tmux session with layout
  case session_manager.create(config.session_name, config.working_dir) {
    Error(err) -> {
      let error_msg = case err {
        session_manager.LayoutError(_) -> "Failed to create tmux layout"
        session_manager.PipeError(_) -> "Failed to create pipes"
        session_manager.NotInitialized -> "Session not initialized"
        session_manager.AgentNotFound(_) -> "Agent not found"
      }
      Error(SessionError(error_msg))
    }
    Ok(manager) -> {
      // Start state store
      case store.start(store.StoreConfig(state_dir: config.state_dir)) {
        Error(_) -> {
          // Clean up session on error
          let _ = session_manager.destroy(manager)
          Error(StoreError("Failed to start state store"))
        }
        Ok(store_subject) -> {
          // Load persisted state
          store.load(store_subject)

          // Create command context
          let command_ctx = commands.CommandContext(store: store_subject)

          // Create REPL config with all commands
          let repl_config =
            repl.default_config()
            |> commands.register_all_commands(command_ctx)

          // Print startup message
          io.println("Scherzo Control REPL")
          io.println("Session: " <> config.session_name)
          io.println("Type 'help' for available commands")
          io.println("")

          // Run the REPL (blocks until quit)
          repl.run(repl_config)

          // Persist state before shutdown
          store.persist(store_subject)
          store.stop(store_subject)

          // Clean up tmux session
          let _ = session_manager.destroy(manager)

          Ok(Nil)
        }
      }
    }
  }
}

/// Start the UI in standalone mode (no tmux session management)
/// Useful for testing or running in an existing terminal
pub fn start_standalone(working_dir: String) -> Result(Nil, RunnerError) {
  let config = default_config(working_dir)
  start(config)
}
