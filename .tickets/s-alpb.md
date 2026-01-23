---
id: s-alpb
status: open
deps: [s-alpa]
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-alp1
tags: [ui, tui, agent-list, phase-2]
---
# Implement keyboard input handling for agent list

Add keyboard input handling to the agent list TUI.

Key bindings:
| Key | Action |
|-----|--------|
| `j` / `↓` | Move selection down |
| `k` / `↑` | Move selection up |
| `Enter` | Focus selected agent's pane (Phase 3) |
| `z` | Zoom selected agent's pane (Phase 3) |
| `Esc` | Return focus to control pane |
| `q` | Quit agent list (exits process, closes pane) |
| `r` | Force refresh agent list |
| `?` | Show help (Phase 4) |

Use glerm's input handling to capture keypresses.

Selection should wrap around (down from last goes to first, up from first goes to last).
