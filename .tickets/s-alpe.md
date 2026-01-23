---
id: s-alpe
status: open
deps: [s-alpd]
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-alp1
tags: [ui, agent-list, phase-3]
---
# Wire z key to zoom toggle

When user presses `z` in the agent list, toggle zoom on the selected agent's pane.

Implementation:
1. Get the selected agent's name from current state
2. Shell out to `scherzo zoom <agent-name>` (or add zoom flag to focus command)
3. Handle errors

When zoomed, the agent pane fills the terminal. Press `z` again to unzoom.

Note: May need to add a `scherzo zoom` command or extend the focus command with a `--zoom` flag.
