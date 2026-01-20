/// Layout management for scherzo tmux UI
///
/// Handles pane creation and organization with a control pane at top
/// and agent panes arranged in a tiled layout below.
import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import scherzo/ui/tmux.{type PaneId, type TmuxError}

/// Purpose/role of a pane in the UI
pub type PaneRole {
  /// Control pane for REPL commands
  ControlPane
  /// Agent pane showing agent output
  AgentPane(agent_id: String)
}

/// Configuration and state for a managed pane
pub type PaneConfig {
  PaneConfig(id: PaneId, role: PaneRole)
}

/// The overall UI layout state
pub type Layout {
  Layout(
    /// The tmux session name
    session: String,
    /// Control pane (always present)
    control_pane: PaneConfig,
    /// Agent panes (may be empty)
    agent_panes: Dict(String, PaneConfig),
  )
}

/// Error during layout operations
pub type LayoutError {
  /// Underlying tmux error
  TmuxError(TmuxError)
  /// No control pane found
  NoControlPane
  /// Agent pane not found
  AgentPaneNotFound(agent_id: String)
  /// Layout already exists
  LayoutExists
}

// ---------------------------------------------------------------------------
// Layout Creation
// ---------------------------------------------------------------------------

/// Create a new layout with just a control pane
/// The session must already exist
pub fn create(session: String) -> Result(Layout, LayoutError) {
  // Get the first pane as control pane
  case tmux.get_first_pane(session) {
    Error(err) -> Error(TmuxError(err))
    Ok(pane_id) -> {
      // Rename window to identify it
      let _ = tmux.rename_window(session, "scherzo")

      let control = PaneConfig(id: pane_id, role: ControlPane)
      Ok(Layout(
        session: session,
        control_pane: control,
        agent_panes: dict.new(),
      ))
    }
  }
}

/// Create a new session with the standard scherzo layout
/// Returns error if session already exists
pub fn create_session_with_layout(
  session: String,
) -> Result(Layout, LayoutError) {
  case tmux.create_session(session) {
    Error(err) -> Error(TmuxError(err))
    Ok(_) -> create(session)
  }
}

// ---------------------------------------------------------------------------
// Pane Management
// ---------------------------------------------------------------------------

/// Add an agent pane to the layout
/// Creates a new pane below the control pane
pub fn add_agent_pane(
  layout: Layout,
  agent_id: String,
) -> Result(Layout, LayoutError) {
  // Split horizontally (new pane below) from the session
  case tmux.split_horizontal(layout.session) {
    Error(err) -> Error(TmuxError(err))
    Ok(pane_id) -> {
      let agent_config = PaneConfig(id: pane_id, role: AgentPane(agent_id))

      // Apply tiled layout to arrange agent panes nicely
      let _ = tmux.apply_tiled_layout(layout.session)

      Ok(Layout(
        ..layout,
        agent_panes: dict.insert(layout.agent_panes, agent_id, agent_config),
      ))
    }
  }
}

/// Add an agent pane that runs a command
pub fn add_agent_pane_with_command(
  layout: Layout,
  agent_id: String,
  command: String,
) -> Result(Layout, LayoutError) {
  // Split with command running in the new pane
  case tmux.split_with_command(layout.session, command, True) {
    Error(err) -> Error(TmuxError(err))
    Ok(pane_id) -> {
      let agent_config = PaneConfig(id: pane_id, role: AgentPane(agent_id))

      // Apply tiled layout
      let _ = tmux.apply_tiled_layout(layout.session)

      Ok(Layout(
        ..layout,
        agent_panes: dict.insert(layout.agent_panes, agent_id, agent_config),
      ))
    }
  }
}

/// Remove an agent pane from the layout
pub fn remove_agent_pane(
  layout: Layout,
  agent_id: String,
) -> Result(Layout, LayoutError) {
  case dict.get(layout.agent_panes, agent_id) {
    Error(_) -> Error(AgentPaneNotFound(agent_id))
    Ok(pane_config) -> {
      // Kill the tmux pane
      case tmux.kill_pane(pane_config.id) {
        Error(err) -> Error(TmuxError(err))
        Ok(_) -> {
          // Re-apply layout after removing pane
          let _ = tmux.apply_tiled_layout(layout.session)

          Ok(Layout(
            ..layout,
            agent_panes: dict.delete(layout.agent_panes, agent_id),
          ))
        }
      }
    }
  }
}

/// Get the pane ID for an agent
pub fn get_agent_pane(layout: Layout, agent_id: String) -> Option(PaneId) {
  case dict.get(layout.agent_panes, agent_id) {
    Ok(config) -> Some(config.id)
    Error(_) -> None
  }
}

/// Get the control pane ID
pub fn get_control_pane(layout: Layout) -> PaneId {
  layout.control_pane.id
}

/// List all agent IDs in the layout
pub fn list_agents(layout: Layout) -> List(String) {
  dict.keys(layout.agent_panes)
}

/// Get the number of agent panes
pub fn agent_count(layout: Layout) -> Int {
  dict.size(layout.agent_panes)
}

// ---------------------------------------------------------------------------
// Pane Focus
// ---------------------------------------------------------------------------

/// Focus the control pane
pub fn focus_control(layout: Layout) -> Result(Nil, LayoutError) {
  case tmux.select_pane(layout.control_pane.id) {
    Error(err) -> Error(TmuxError(err))
    Ok(_) -> Ok(Nil)
  }
}

/// Focus an agent pane
pub fn focus_agent(layout: Layout, agent_id: String) -> Result(Nil, LayoutError) {
  case dict.get(layout.agent_panes, agent_id) {
    Error(_) -> Error(AgentPaneNotFound(agent_id))
    Ok(config) -> {
      case tmux.select_pane(config.id) {
        Error(err) -> Error(TmuxError(err))
        Ok(_) -> Ok(Nil)
      }
    }
  }
}

/// Zoom/maximize an agent pane (toggle)
pub fn zoom_agent(layout: Layout, agent_id: String) -> Result(Nil, LayoutError) {
  case dict.get(layout.agent_panes, agent_id) {
    Error(_) -> Error(AgentPaneNotFound(agent_id))
    Ok(config) -> {
      case tmux.zoom_pane(config.id) {
        Error(err) -> Error(TmuxError(err))
        Ok(_) -> Ok(Nil)
      }
    }
  }
}

// ---------------------------------------------------------------------------
// Layout Helpers
// ---------------------------------------------------------------------------

/// Apply main-horizontal layout (control pane large at top)
pub fn apply_main_layout(layout: Layout) -> Result(Nil, LayoutError) {
  // Set main pane height for control pane (about 30% of terminal)
  let _ = tmux.set_main_pane_height(layout.session, 15)

  case tmux.apply_main_horizontal_layout(layout.session) {
    Error(err) -> Error(TmuxError(err))
    Ok(_) -> Ok(Nil)
  }
}

/// Apply tiled layout (all panes roughly equal)
pub fn apply_tiled(layout: Layout) -> Result(Nil, LayoutError) {
  case tmux.apply_tiled_layout(layout.session) {
    Error(err) -> Error(TmuxError(err))
    Ok(_) -> Ok(Nil)
  }
}

/// Destroy the layout (kills the session)
pub fn destroy(layout: Layout) -> Result(Nil, LayoutError) {
  case tmux.kill_session(layout.session) {
    Error(err) -> Error(TmuxError(err))
    Ok(_) -> Ok(Nil)
  }
}
