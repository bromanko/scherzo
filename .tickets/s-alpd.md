---
id: s-alpd
status: open
deps: [s-4c84, s-alpb]
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-alp1
tags: [ui, agent-list, phase-3]
---
# Wire Enter key to focus command

When user presses Enter in the agent list, focus the selected agent's pane.

Implementation:
1. Get the selected agent's name from current state
2. Shell out to `scherzo focus <agent-name>`
3. Handle errors (agent not found, focus failed)

This depends on s-4c84 (wire focus command to layout manager) being completed first.

After focusing, the agent list remains visible but the cursor/focus moves to the agent pane.
