/// UI Runner - wires together REPL, commands, and ticket system
///
/// Provides the startup sequence for the scherzo tmux UI,
/// using the ticket system as the source of truth for tasks.
import gleam/io
import scherzo/ui/commands
import scherzo/ui/repl
import scherzo/ui/session_manager

/// Configuration for the UI runner
pub type RunnerConfig {
  RunnerConfig(
    /// Session name for tmux
    session_name: String,
    /// Working directory (contains .tickets/)
    working_dir: String,
  )
}

/// Default runner config
pub fn default_config(working_dir: String) -> RunnerConfig {
  RunnerConfig(session_name: "scherzo", working_dir: working_dir)
}

/// Error during UI startup
pub type RunnerError {
  /// Failed to create tmux session
  SessionError(String)
}

/// Start the UI with all components wired together
/// This function blocks until the REPL exits
pub fn start(config: RunnerConfig) -> Result(Nil, RunnerError) {
  // Create command context with working directory
  // Tasks are queried directly from .tickets/ (single source of truth)
  let command_ctx = commands.CommandContext(working_dir: config.working_dir)

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

  Ok(Nil)
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
      // Create command context with working directory
      // Tasks are queried directly from .tickets/ (single source of truth)
      let command_ctx = commands.CommandContext(working_dir: config.working_dir)

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

      // Clean up tmux session
      let _ = session_manager.destroy(manager)

      Ok(Nil)
    }
  }
}

/// Start the UI in standalone mode (no tmux session management)
/// Useful for testing or running in an existing terminal
pub fn start_standalone(working_dir: String) -> Result(Nil, RunnerError) {
  let config = default_config(working_dir)
  start(config)
}
