/// Agent List TUI module
///
/// Renders a terminal UI showing all running agents with keyboard navigation.
/// Uses ANSI escape codes for terminal control.
import gleam/int
import gleam/list
import gleam/option.{type Option, None}
import gleam/result
import gleam/string
import scherzo/core/names
import scherzo/core/types
import scherzo/state/agents as agents_state

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// Agent info parsed from JSON
pub type AgentInfo {
  AgentInfo(
    id: String,
    name: String,
    status: String,
    task_id: String,
    provider: String,
  )
}

/// TUI state
pub type State {
  State(
    /// List of agents from last poll
    agents: List(AgentInfo),
    /// Currently selected index (0-based)
    selected: Int,
    /// Error message from last poll (if any)
    error: Option(String),
    /// Terminal width (for rendering)
    width: Int,
  )
}

// ---------------------------------------------------------------------------
// ANSI Escape Codes
// ---------------------------------------------------------------------------

/// Clear screen and move cursor to top-left
pub fn clear_screen() -> String {
  "\u{001b}[2J\u{001b}[H"
}

/// Move cursor to position (1-based)
pub fn move_cursor(row: Int, col: Int) -> String {
  "\u{001b}[" <> int.to_string(row) <> ";" <> int.to_string(col) <> "H"
}

/// Hide cursor
pub fn hide_cursor() -> String {
  "\u{001b}[?25l"
}

/// Show cursor
pub fn show_cursor() -> String {
  "\u{001b}[?25h"
}

/// Invert colors (for selection highlight)
pub fn invert() -> String {
  "\u{001b}[7m"
}

/// Reset text attributes
pub fn reset() -> String {
  "\u{001b}[0m"
}

/// Bold text
pub fn bold() -> String {
  "\u{001b}[1m"
}

/// Dim text
pub fn dim() -> String {
  "\u{001b}[2m"
}

/// Green text (for running status)
pub fn green() -> String {
  "\u{001b}[32m"
}

/// Yellow text (for waiting status)
pub fn yellow() -> String {
  "\u{001b}[33m"
}

/// Red text (for failed status)
pub fn red() -> String {
  "\u{001b}[31m"
}

/// Cyan text (for idle status)
pub fn cyan() -> String {
  "\u{001b}[36m"
}

// ---------------------------------------------------------------------------
// State Management
// ---------------------------------------------------------------------------

/// Create initial state
pub fn initial_state() -> State {
  State(agents: [], selected: 0, error: None, width: 60)
}

/// Update agents from working directory
pub fn update_agents(state: State, working_dir: String) -> State {
  let agents_dir = working_dir <> "/" <> agents_state.default_agents_dir
  let all_agents = agents_state.load_all_agents(agents_dir)

  let agents =
    all_agents
    |> list.map(fn(agent) {
      let friendly_name = names.from_agent_id(agent.config.id)
      let status_str = format_status(agent.status)
      let provider = format_provider(agent.config.provider)

      AgentInfo(
        id: agent.config.id,
        name: friendly_name,
        status: status_str,
        task_id: agent.task_id,
        provider: provider,
      )
    })

  // Clamp selected index to valid range
  let max_index = case list.length(agents) {
    0 -> 0
    n -> n - 1
  }
  let selected = case state.selected > max_index {
    True -> max_index
    False -> state.selected
  }

  State(..state, agents: agents, error: None, selected: selected)
}

/// Format agent status
fn format_status(status: types.AgentStatus) -> String {
  case status {
    types.Idle -> "idle"
    types.Running(_, _) -> "running"
    types.Completed(_, _) -> "completed"
    types.Failed(_) -> "failed"
  }
}

/// Format provider name
fn format_provider(provider: types.AgentProvider) -> String {
  case provider {
    types.Claude -> "claude"
    types.Codex -> "codex"
    types.Gemini -> "gemini"
  }
}

/// Move selection up
pub fn select_prev(state: State) -> State {
  let selected = case state.selected > 0 {
    True -> state.selected - 1
    False -> state.selected
  }
  State(..state, selected: selected)
}

/// Move selection down
pub fn select_next(state: State) -> State {
  let max_index = case list.length(state.agents) {
    0 -> 0
    n -> n - 1
  }
  let selected = case state.selected < max_index {
    True -> state.selected + 1
    False -> state.selected
  }
  State(..state, selected: selected)
}

/// Get currently selected agent ID
pub fn get_selected_agent(state: State) -> Option(String) {
  state.agents
  |> list.drop(state.selected)
  |> list.first
  |> result.map(fn(agent) { agent.id })
  |> option.from_result
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

/// Render the full TUI to a string
pub fn render(state: State) -> String {
  let width = state.width

  // Build output
  let output = string.concat([
    clear_screen(),
    hide_cursor(),
    render_header(state, width),
    "\n",
    render_agents(state, width),
    "\n",
    render_footer(width),
  ])

  output
}

/// Render header with agent count
fn render_header(state: State, width: Int) -> String {
  let count = list.length(state.agents)
  let title = " Agents (" <> int.to_string(count) <> ") "

  // Build the header line: ┌─ Agents (3) ─────────────────┐
  let top_left = "┌─"
  let top_right = "─┐"
  let fill_len = width - string.length(top_left) - string.length(title) - string.length(top_right)
  let fill = string.repeat("─", case fill_len > 0 {
    True -> fill_len
    False -> 0
  })

  bold() <> top_left <> title <> fill <> top_right <> reset()
}

/// Render agent list with selection highlighting
fn render_agents(state: State, width: Int) -> String {
  case state.agents {
    [] -> render_empty_message(width)
    agents -> {
      agents
      |> list.index_map(fn(agent, index) {
        render_agent_row(agent, index == state.selected, width)
      })
      |> string.join("\n")
    }
  }
}

/// Render empty state message
fn render_empty_message(width: Int) -> String {
  let msg = " No agents running "
  let padding = case width - string.length(msg) - 4 {
    n if n > 0 -> string.repeat(" ", n)
    _ -> ""
  }
  "│" <> dim() <> msg <> reset() <> padding <> " │"
}

/// Render a single agent row
fn render_agent_row(agent: AgentInfo, is_selected: Bool, width: Int) -> String {
  // Status indicator
  let status_indicator = case agent.status {
    "running" -> green() <> "●" <> reset()
    "idle" -> cyan() <> "○" <> reset()
    "completed" -> green() <> "✓" <> reset()
    "failed" -> red() <> "✗" <> reset()
    _ -> "◐"
  }

  // Selection marker
  let marker = case is_selected {
    True -> "▶ "
    False -> "  "
  }

  // Build the row content
  let content =
    marker
    <> pad_right(agent.name, 16)
    <> " "
    <> status_indicator
    <> " "
    <> pad_right(agent.status, 10)
    <> " "
    <> truncate(agent.task_id, 25)

  // Calculate padding for box border
  let content_len = visible_length(content)
  let padding_len = width - content_len - 4  // 4 for "│ " and " │"
  let padding = case padding_len > 0 {
    True -> string.repeat(" ", padding_len)
    False -> ""
  }

  // Wrap in box borders with optional invert for selection
  case is_selected {
    True -> "│ " <> invert() <> content <> padding <> reset() <> " │"
    False -> "│ " <> content <> padding <> " │"
  }
}

/// Render footer with keybindings
fn render_footer(width: Int) -> String {
  let bottom_line = "└" <> string.repeat("─", width - 2) <> "┘"
  let help = dim() <> " ↑↓ select  Enter focus  q quit" <> reset()

  bottom_line <> "\n" <> help
}

/// Pad string to the right to given width
fn pad_right(s: String, width: Int) -> String {
  let len = string.length(s)
  case len < width {
    True -> s <> string.repeat(" ", width - len)
    False -> string.slice(s, 0, width)
  }
}

/// Truncate string to max length with ellipsis
fn truncate(s: String, max_len: Int) -> String {
  case string.length(s) > max_len {
    True -> string.slice(s, 0, max_len - 3) <> "..."
    False -> pad_right(s, max_len)
  }
}

/// Calculate visible length (excluding ANSI codes)
fn visible_length(s: String) -> Int {
  // Simple approximation: remove escape sequences
  s
  |> string.replace("\u{001b}[0m", "")
  |> string.replace("\u{001b}[1m", "")
  |> string.replace("\u{001b}[2m", "")
  |> string.replace("\u{001b}[7m", "")
  |> string.replace("\u{001b}[31m", "")
  |> string.replace("\u{001b}[32m", "")
  |> string.replace("\u{001b}[33m", "")
  |> string.replace("\u{001b}[36m", "")
  |> string.length
}
