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
  /// Agent list pane showing all agents
  AgentListPane
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
    /// Agent list pane (optional, toggleable with F1)
    agent_list_pane: Option(PaneConfig),
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
  /// Agent list pane already exists
  AgentListPaneExists
  /// Agent list pane not found
  AgentListPaneNotFound
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
        agent_list_pane: None,
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

/// Create a new session with a command running in the control pane
/// This is used for `scherzo console` to run the REPL inside tmux
pub fn create_session_with_command(
  session: String,
  command: String,
) -> Result(Layout, LayoutError) {
  case tmux.create_session_with_command(session, command) {
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

      Ok(
        Layout(
          ..layout,
          agent_panes: dict.insert(layout.agent_panes, agent_id, agent_config),
        ),
      )
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

      Ok(
        Layout(
          ..layout,
          agent_panes: dict.insert(layout.agent_panes, agent_id, agent_config),
        ),
      )
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

          Ok(
            Layout(
              ..layout,
              agent_panes: dict.delete(layout.agent_panes, agent_id),
            ),
          )
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

// ---------------------------------------------------------------------------
// Agent List Pane
// ---------------------------------------------------------------------------

/// Minimum height for agent list pane
const agent_list_min_height = 3

/// Default command for agent list pane
const agent_list_command = "scherzo agent-list"

/// Create agent list pane at bottom with fixed height
/// Runs `scherzo agent-list` command in the new pane
pub fn create_agent_list_pane(
  layout: Layout,
  height: Int,
) -> Result(Layout, LayoutError) {
  create_agent_list_pane_with_command(layout, height, agent_list_command)
}

/// Create agent list pane with a custom command
/// Used internally and for testing
pub fn create_agent_list_pane_with_command(
  layout: Layout,
  height: Int,
  command: String,
) -> Result(Layout, LayoutError) {
  // Check if agent list pane already exists
  case layout.agent_list_pane {
    Some(_) -> Error(AgentListPaneExists)
    None -> {
      // Ensure height is at least minimum
      let actual_height = case height < agent_list_min_height {
        True -> agent_list_min_height
        False -> height
      }

      // Split at bottom with the command
      case tmux.split_with_command(layout.session, command, True) {
        Error(err) -> Error(TmuxError(err))
        Ok(pane_id) -> {
          // Resize to fixed height
          case tmux.resize_pane_height(pane_id, actual_height) {
            Error(err) -> {
              // Clean up the pane if resize fails
              let _ = tmux.kill_pane(pane_id)
              Error(TmuxError(err))
            }
            Ok(_) -> {
              let config = PaneConfig(id: pane_id, role: AgentListPane)
              Ok(Layout(..layout, agent_list_pane: Some(config)))
            }
          }
        }
      }
    }
  }
}

/// Destroy agent list pane
pub fn destroy_agent_list_pane(layout: Layout) -> Result(Layout, LayoutError) {
  case layout.agent_list_pane {
    None -> Error(AgentListPaneNotFound)
    Some(config) -> {
      case tmux.kill_pane(config.id) {
        Error(err) -> Error(TmuxError(err))
        Ok(_) -> Ok(Layout(..layout, agent_list_pane: None))
      }
    }
  }
}

/// Toggle agent list pane visibility
/// Creates the pane if hidden, destroys it if visible
pub fn toggle_agent_list_pane(
  layout: Layout,
  height: Int,
) -> Result(Layout, LayoutError) {
  toggle_agent_list_pane_with_command(layout, height, agent_list_command)
}

/// Toggle agent list pane with a custom command
/// Used internally and for testing
pub fn toggle_agent_list_pane_with_command(
  layout: Layout,
  height: Int,
  command: String,
) -> Result(Layout, LayoutError) {
  case layout.agent_list_pane {
    None -> create_agent_list_pane_with_command(layout, height, command)
    Some(_) -> destroy_agent_list_pane(layout)
  }
}

/// Get the agent list pane ID if it exists
pub fn get_agent_list_pane(layout: Layout) -> Option(PaneId) {
  case layout.agent_list_pane {
    Some(config) -> Some(config.id)
    None -> None
  }
}

/// Check if agent list pane is visible
pub fn has_agent_list_pane(layout: Layout) -> Bool {
  option.is_some(layout.agent_list_pane)
}
