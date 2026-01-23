---
id: s-alp1
status: open
deps: []
links: [docs/agent-list-pane.md]
created: 2026-01-23T00:00:00Z
type: epic
priority: 2
assignee: Brian Romanko
tags: [ui, tmux, agent-list]
---
# Agent List Pane

A persistent tmux pane showing all running agents with keyboard navigation for quick focus switching.

The agent list pane:
- Lives at the bottom of the tmux layout (small fixed height, toggleable with F1)
- Shows all agents with name, status, and task ID
- Highlights currently selected agent
- Keyboard navigation: up/down to select, Enter to focus that agent's pane

See `docs/agent-list-pane.md` for full design doc.

## Phases

1. **Layout System Changes** - Add fixed-height pane support to layout system
2. **Agent List Binary** - Create `scherzo agent-list` TUI with glerm
3. **Focus Integration** - Wire Enter key to focus command
4. **Polish** - Scrolling, help screen, resize handling
