---
id: s-alp7
status: done
deps: [s-alp3, s-alp5]
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-alp1
tags: [ui, tmux, agent-list, phase-1]
---
# Add F1 key binding for agent list toggle

Register F1 as a tmux key binding to toggle the agent list pane visibility.

When F1 is pressed:
- If agent list pane is hidden: create it with `scherzo agent-list`
- If agent list pane is visible: kill it

Implementation options:
1. Bind F1 to a scherzo CLI command: `scherzo toggle-agent-list`
2. Bind F1 to inline tmux commands that check pane existence

Option 1 is cleaner and allows the toggle logic to live in Gleam code.

The binding should be registered when the tmux session is created in `create_session_with_layout`.
