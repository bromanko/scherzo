/// Session manager for scherzo tmux UI
///
/// Coordinates layout and pipe management for dynamic agent pane handling.
/// Provides a unified interface for creating/removing agent panes with
/// proper output routing setup and cleanup.
import gleam/option.{type Option, None, Some}
import scherzo/ui/layout.{type Layout, type LayoutError}
import scherzo/ui/pipes.{type PipeConfig, type PipeError}

/// Session manager state
pub type SessionManager {
  SessionManager(
    /// Current layout state
    layout: Layout,
    /// Pipe configuration
    pipe_config: PipeConfig,
    /// Working directory for the session
    working_dir: String,
  )
}

/// Error during session management
pub type SessionError {
  /// Layout operation failed
  LayoutError(LayoutError)
  /// Pipe operation failed
  PipeError(PipeError)
  /// Session not initialized
  NotInitialized
  /// Agent not found
  AgentNotFound(agent_id: String)
}

// ---------------------------------------------------------------------------
// Session Lifecycle
// ---------------------------------------------------------------------------

/// Create a new session manager with a fresh tmux session
/// This creates the session, layout, and pipes directory
pub fn create(
  session_name: String,
  working_dir: String,
) -> Result(SessionManager, SessionError) {
  // Create pipe config
  let pipe_config = pipes.default_config(working_dir)

  // Ensure pipes directory exists
  case pipes.ensure_pipes_dir(pipe_config) {
    Error(err) -> Error(PipeError(err))
    Ok(_) -> {
      // Create tmux session with layout
      case layout.create_session_with_layout(session_name) {
        Error(err) -> Error(LayoutError(err))
        Ok(l) ->
          Ok(SessionManager(
            layout: l,
            pipe_config: pipe_config,
            working_dir: working_dir,
          ))
      }
    }
  }
}

/// Create a session manager for an existing tmux session
pub fn attach_to_session(
  session_name: String,
  working_dir: String,
) -> Result(SessionManager, SessionError) {
  // Create pipe config
  let pipe_config = pipes.default_config(working_dir)

  // Ensure pipes directory exists
  case pipes.ensure_pipes_dir(pipe_config) {
    Error(err) -> Error(PipeError(err))
    Ok(_) -> {
      // Create layout for existing session
      case layout.create(session_name) {
        Error(err) -> Error(LayoutError(err))
        Ok(l) ->
          Ok(SessionManager(
            layout: l,
            pipe_config: pipe_config,
            working_dir: working_dir,
          ))
      }
    }
  }
}

/// Destroy the session and clean up all resources
pub fn destroy(manager: SessionManager) -> Result(Nil, SessionError) {
  // Clean up all pipes first
  let _ = pipes.cleanup_all_pipes(manager.pipe_config)

  // Then destroy the tmux session
  case layout.destroy(manager.layout) {
    Error(err) -> Error(LayoutError(err))
    Ok(_) -> Ok(Nil)
  }
}

// ---------------------------------------------------------------------------
// Agent Pane Management
// ---------------------------------------------------------------------------

/// Add a new agent pane with output routing
/// This creates:
/// 1. A named pipe for the agent's output
/// 2. A tmux pane that tails the pipe
/// Returns updated manager and the pipe path
pub fn add_agent(
  manager: SessionManager,
  agent_id: String,
) -> Result(#(SessionManager, String), SessionError) {
  // Create the pipe first
  case pipes.create_pipe(manager.pipe_config, agent_id) {
    Error(err) -> Error(PipeError(err))
    Ok(pipe_path) -> {
      // Get the tail command for the pane
      let tail_cmd = pipes.tail_command(manager.pipe_config, agent_id)

      // Create pane running tail command
      case
        layout.add_agent_pane_with_command(manager.layout, agent_id, tail_cmd)
      {
        Error(err) -> {
          // Clean up the pipe if pane creation fails
          let _ = pipes.remove_pipe(manager.pipe_config, agent_id)
          Error(LayoutError(err))
        }
        Ok(new_layout) -> {
          let new_manager = SessionManager(..manager, layout: new_layout)
          Ok(#(new_manager, pipe_path))
        }
      }
    }
  }
}

/// Remove an agent pane and clean up its resources
/// This:
/// 1. Kills the tmux pane
/// 2. Removes the named pipe
pub fn remove_agent(
  manager: SessionManager,
  agent_id: String,
) -> Result(SessionManager, SessionError) {
  // Remove the pane first
  case layout.remove_agent_pane(manager.layout, agent_id) {
    Error(err) -> Error(LayoutError(err))
    Ok(new_layout) -> {
      // Clean up the pipe (ignore errors - pipe may already be gone)
      let _ = pipes.remove_pipe(manager.pipe_config, agent_id)

      let new_manager = SessionManager(..manager, layout: new_layout)
      Ok(new_manager)
    }
  }
}

/// Get the pipe path for an agent
pub fn get_agent_pipe(
  manager: SessionManager,
  agent_id: String,
) -> Option(String) {
  case layout.get_agent_pane(manager.layout, agent_id) {
    None -> None
    Some(_) -> Some(pipes.pipe_path(manager.pipe_config, agent_id))
  }
}

/// List all active agent IDs
pub fn list_agents(manager: SessionManager) -> List(String) {
  layout.list_agents(manager.layout)
}

/// Get the number of active agents
pub fn agent_count(manager: SessionManager) -> Int {
  layout.agent_count(manager.layout)
}

// ---------------------------------------------------------------------------
// Focus Management
// ---------------------------------------------------------------------------

/// Focus the control pane
pub fn focus_control(manager: SessionManager) -> Result(Nil, SessionError) {
  case layout.focus_control(manager.layout) {
    Error(err) -> Error(LayoutError(err))
    Ok(_) -> Ok(Nil)
  }
}

/// Focus an agent pane
pub fn focus_agent(
  manager: SessionManager,
  agent_id: String,
) -> Result(Nil, SessionError) {
  case layout.focus_agent(manager.layout, agent_id) {
    Error(err) -> Error(LayoutError(err))
    Ok(_) -> Ok(Nil)
  }
}

/// Zoom/maximize an agent pane (toggle)
pub fn zoom_agent(
  manager: SessionManager,
  agent_id: String,
) -> Result(Nil, SessionError) {
  case layout.zoom_agent(manager.layout, agent_id) {
    Error(err) -> Error(LayoutError(err))
    Ok(_) -> Ok(Nil)
  }
}

// ---------------------------------------------------------------------------
// Layout Helpers
// ---------------------------------------------------------------------------

/// Apply main-horizontal layout (control pane large at top)
pub fn apply_main_layout(manager: SessionManager) -> Result(Nil, SessionError) {
  case layout.apply_main_layout(manager.layout) {
    Error(err) -> Error(LayoutError(err))
    Ok(_) -> Ok(Nil)
  }
}

/// Apply tiled layout (all panes roughly equal)
pub fn apply_tiled(manager: SessionManager) -> Result(Nil, SessionError) {
  case layout.apply_tiled(manager.layout) {
    Error(err) -> Error(LayoutError(err))
    Ok(_) -> Ok(Nil)
  }
}

/// Get the session name
pub fn session_name(manager: SessionManager) -> String {
  manager.layout.session
}

/// Get the control pane ID
pub fn control_pane_id(manager: SessionManager) -> String {
  layout.get_control_pane(manager.layout)
}
