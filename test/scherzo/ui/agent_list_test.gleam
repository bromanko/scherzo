import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import scherzo/ui/agent_list

// ---------------------------------------------------------------------------
// State Tests
// ---------------------------------------------------------------------------

pub fn initial_state_has_no_agents_test() {
  let state = agent_list.initial_state()
  state.agents
  |> should.equal([])
}

pub fn initial_state_has_selected_zero_test() {
  let state = agent_list.initial_state()
  state.selected
  |> should.equal(0)
}

pub fn initial_state_has_no_error_test() {
  let state = agent_list.initial_state()
  state.error
  |> should.equal(None)
}

// ---------------------------------------------------------------------------
// Selection Tests
// ---------------------------------------------------------------------------

pub fn select_next_increments_selected_test() {
  let state = agent_list.State(
    agents: [
      agent_list.AgentInfo("id-1", "name-1", "running", "task-1", "claude"),
      agent_list.AgentInfo("id-2", "name-2", "idle", "task-2", "claude"),
    ],
    selected: 0,
    error: None,
    width: 60,
  )

  let state = agent_list.select_next(state)
  state.selected
  |> should.equal(1)
}

pub fn select_next_stops_at_end_test() {
  let state = agent_list.State(
    agents: [
      agent_list.AgentInfo("id-1", "name-1", "running", "task-1", "claude"),
      agent_list.AgentInfo("id-2", "name-2", "idle", "task-2", "claude"),
    ],
    selected: 1,
    error: None,
    width: 60,
  )

  let state = agent_list.select_next(state)
  state.selected
  |> should.equal(1)
}

pub fn select_prev_decrements_selected_test() {
  let state = agent_list.State(
    agents: [
      agent_list.AgentInfo("id-1", "name-1", "running", "task-1", "claude"),
      agent_list.AgentInfo("id-2", "name-2", "idle", "task-2", "claude"),
    ],
    selected: 1,
    error: None,
    width: 60,
  )

  let state = agent_list.select_prev(state)
  state.selected
  |> should.equal(0)
}

pub fn select_prev_stops_at_zero_test() {
  let state = agent_list.State(
    agents: [
      agent_list.AgentInfo("id-1", "name-1", "running", "task-1", "claude"),
    ],
    selected: 0,
    error: None,
    width: 60,
  )

  let state = agent_list.select_prev(state)
  state.selected
  |> should.equal(0)
}

pub fn get_selected_agent_returns_correct_id_test() {
  let state = agent_list.State(
    agents: [
      agent_list.AgentInfo("id-1", "name-1", "running", "task-1", "claude"),
      agent_list.AgentInfo("id-2", "name-2", "idle", "task-2", "claude"),
    ],
    selected: 1,
    error: None,
    width: 60,
  )

  agent_list.get_selected_agent(state)
  |> should.equal(Some("id-2"))
}

pub fn get_selected_agent_returns_none_for_empty_test() {
  let state = agent_list.initial_state()

  agent_list.get_selected_agent(state)
  |> should.equal(None)
}

// ---------------------------------------------------------------------------
// Render Tests
// ---------------------------------------------------------------------------

pub fn render_includes_header_test() {
  let state = agent_list.initial_state()
  let output = agent_list.render(state)

  string.contains(output, "Agents")
  |> should.be_true
}

pub fn render_includes_empty_message_test() {
  let state = agent_list.initial_state()
  let output = agent_list.render(state)

  string.contains(output, "No agents running")
  |> should.be_true
}

pub fn render_includes_agent_name_test() {
  let state = agent_list.State(
    agents: [
      agent_list.AgentInfo("id-1", "test-agent", "running", "task-1", "claude"),
    ],
    selected: 0,
    error: None,
    width: 60,
  )

  let output = agent_list.render(state)

  string.contains(output, "test-agent")
  |> should.be_true
}

pub fn render_includes_status_test() {
  let state = agent_list.State(
    agents: [
      agent_list.AgentInfo("id-1", "test-agent", "running", "task-1", "claude"),
    ],
    selected: 0,
    error: None,
    width: 60,
  )

  let output = agent_list.render(state)

  string.contains(output, "running")
  |> should.be_true
}

pub fn render_includes_keybinding_help_test() {
  let state = agent_list.initial_state()
  let output = agent_list.render(state)

  string.contains(output, "select")
  |> should.be_true

  string.contains(output, "quit")
  |> should.be_true
}

// ---------------------------------------------------------------------------
// ANSI Code Tests
// ---------------------------------------------------------------------------

pub fn clear_screen_returns_escape_code_test() {
  let code = agent_list.clear_screen()

  // Should contain escape character
  string.contains(code, "\u{001b}")
  |> should.be_true
}

pub fn hide_cursor_returns_escape_code_test() {
  let code = agent_list.hide_cursor()

  string.contains(code, "\u{001b}")
  |> should.be_true
}

pub fn reset_returns_escape_code_test() {
  let code = agent_list.reset()

  string.contains(code, "\u{001b}")
  |> should.be_true
}
