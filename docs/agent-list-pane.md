# Agent List Pane

A persistent tmux pane showing all running agents with keyboard navigation for quick focus switching.

## Overview

```
┌─────────────────────────────────────────────────────────────┐
│ Control Pane (REPL)                                         │
│ scherzo> agents                                             │
│ ...                                                         │
├─────────────────────────────────────────────────────────────┤
│ Agent: fine-elk-32                    │ Agent: cool-fox-17  │
│ claude --resume ...                   │ claude --resume ... │
│ ...                                   │ ...                 │
├───────────────────────────────────────┴─────────────────────┤
│ Agents ─────────────────────────────────────────────────────│
│   fine-elk-32    running   task-1769208524758494003         │
│ ▶ cool-fox-17    idle      task-1769210740397194018         │
│   swift-owl-88   waiting   task-1769211234567890123         │
└─────────────────────────────────────────────────────────────┘
```

The agent list pane:
- Lives at the bottom of the tmux layout (small fixed height)
- Shows all agents with name, status, and task ID
- Highlights currently selected agent
- Keyboard navigation: up/down to select, Enter to focus that agent's pane

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Pane location | Bottom | Consistent with status bars; doesn't compete with agent panes |
| Pane height | 5-7 rows fixed (min 3) | Enough for ~4 agents visible + header; scrollable if more |
| Implementation | Gleam TUI with glerm | Consistent with codebase; glerm provides terminal primitives |
| State source | Poll via CLI | Call `scherzo agents --json` every 1s; reuses existing code |
| Selection persistence | In-memory only | Focus is ephemeral; no need to persist across restarts |
| Key bindings | vim-style (j/k) + arrows | Familiar to target users; arrows for discoverability |
| Enter behavior | Focus + zoom toggle | Most common action is wanting to see an agent's output |
| Escape behavior | Always return to control | Simple and predictable; Esc always goes to REPL |
| Auto-scroll | Follow new agents | Keep newest agents visible; user can scroll back |
| Visibility | Toggle with F1 | Can be hidden to save screen space |
| Small terminal | Maintain minimum height | Keep 2-3 row minimum even if content clips |
| Error handling | Show inline | Display error in pane, retry on next poll cycle |

## Pane Lifecycle

### Creation

The agent list pane is created when the console starts:

1. `scherzo console` creates tmux session
2. Control pane runs the REPL
3. Agent list pane is split at bottom with fixed height
4. Agent list pane runs `scherzo agent-list` (new subcommand)

### Layout Integration

```gleam
// layout.gleam changes:

pub type Layout {
  Layout(
    session: String,
    control_pane: PaneConfig,
    agent_panes: Dict(String, PaneConfig),
    agent_list_pane: Option(PaneConfig),  // NEW
  )
}

// tmux.gleam additions:

/// Resize a pane to a specific height
pub fn resize_pane_height(pane_id: PaneId, height: Int) -> Result(Nil, TmuxError)

/// Bind a key at the session level
pub fn bind_key(session: String, key: String, command: String) -> Result(Nil, TmuxError)
```

The agent list pane is **not** included in tiled layout calculations. After `apply_tiled_layout` runs on agent panes, we re-apply the fixed height to the agent list pane.

### Toggle Visibility

The agent list pane can be toggled with F1:
- **Show**: If hidden, create pane at bottom with `scherzo agent-list`
- **Hide**: If visible, kill the pane (state is ephemeral)

This is implemented as a tmux key binding so it works from any pane.

### Destruction

Agent list pane is killed when:
- Session is destroyed (`scherzo console` exits)
- User presses F1 to hide it
- User presses `q` from within the agent list pane

## Agent List Binary

New subcommand: `scherzo agent-list`

### Responsibilities

1. Poll agent state via `scherzo agents --json` (every 1 second)
2. Render agent list with TUI (using glerm)
3. Handle keyboard input for navigation and actions
4. Send focus commands via `scherzo focus <agent-name>`

### Rendering

```
┌─ Agents (3) ────────────────────────────────────────────────┐
│   fine-elk-32    ● running   task-1769208524758494003       │
│ ▶ cool-fox-17    ○ idle      task-1769210740397194018       │
│   swift-owl-88   ◐ waiting   task-1769211234567890123       │
└─────────────────────────────────────────────────────────────┘
```

- Header shows agent count
- Status indicators: `●` running, `○` idle, `◐` waiting for input, `✓` done, `✗` failed
- Task ID shown (truncated if needed)
- Selected row highlighted (inverted colors or `▶` marker)

### Key Bindings

| Key | Action |
|-----|--------|
| `j` / `↓` | Move selection down |
| `k` / `↑` | Move selection up |
| `Enter` | Focus selected agent's pane |
| `z` | Zoom selected agent's pane (toggle) |
| `Esc` | Return focus to control pane |
| `q` | Quit agent list (closes pane) |
| `r` | Refresh agent list |
| `?` | Show help |

### Global Toggle (from any pane)

| Key | Action |
|-----|--------|
| `F1` | Toggle agent list pane visibility |

Note: F1 binding is registered at the tmux level so it works from any pane.

### Communication

The agent list needs to:
1. **Read** agent state - call `scherzo agents --json` every 1 second
2. **Send** focus commands - call `scherzo focus <agent-name>` (s-4c84)

This approach reuses existing CLI infrastructure and avoids new IPC protocols.

## Implementation Plan

### Phase 1: Layout System Changes

The current layout system uses `tmux select-layout tiled` which redistributes all panes equally. Fixed-height panes are not currently supported.

1. Add `resize_pane_height(pane_id, height)` to `tmux.gleam`
2. Add `agent_list_pane: Option(PaneConfig)` field to `Layout` type
3. Add `create_agent_list_pane()` function that:
   - Splits at bottom with fixed height (5-7 rows)
   - Runs `scherzo agent-list` command
4. Modify `apply_tiled_layout` to exclude agent list pane:
   - Apply tiled to agent panes only
   - Re-apply fixed height to agent list pane after tiling
5. Add F1 tmux key binding to toggle agent list visibility

### Phase 2: Agent List Binary

1. Create `src/scherzo/ui/agent_list.gleam` module
2. Add `scherzo agent-list` subcommand to CLI
3. Implement TUI rendering with glerm:
   - Header with agent count
   - Agent rows with name, status indicator, task ID
   - Selection highlighting
4. Implement keyboard input handling (j/k/arrows, Enter, Esc, z, q, r, ?)
5. Poll `scherzo agents --json` every 1 second
6. Add `--json` flag to existing `agents` command for structured output

### Phase 3: Focus Integration

1. Complete s-4c84 (wire focus command to layout manager)
2. Agent list calls `scherzo focus <name>` on Enter
3. Handle zoom toggle with `z` key

### Phase 4: Polish

1. Add scrolling for many agents (page up/down)
2. Add help screen (`?` key)
3. Handle terminal resize gracefully (maintain minimum height)

## Error Handling

When errors occur (CLI call fails, malformed JSON, etc.):
- Display error message inline in the agent list pane
- Retry on next poll cycle (1 second)
- Example: `Error: Failed to fetch agents - retrying...`

## Resolved Questions

| Question | Decision |
|----------|----------|
| TUI library | glerm - Gleam-native terminal library |
| State polling interval | 1 second - good balance of responsiveness and efficiency |
| Optional or always present | Toggle with F1 hotkey |
| Horizontal split vs overlay | Horizontal split (pane takes space at bottom) |

## Related Tickets

- s-4c84: Wire focus command to layout manager (prerequisite)
- s-417f: Add 'WaitingForInput' agent status (enhances display)
