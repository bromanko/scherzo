---
id: s-alpi
status: open
deps: [s-alpa]
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 3
assignee: Brian Romanko
parent: s-alp1
tags: [ui, agent-list, phase-4]
---
# Handle terminal resize gracefully

When the terminal is resized, the agent list should adapt:

1. Re-render to fit new dimensions
2. Maintain minimum height of 2-3 rows even if terminal is very small
3. Adjust viewport/scrolling if visible area changes
4. Preserve current selection

Implementation:
- Listen for SIGWINCH signal (terminal resize)
- Query new terminal dimensions
- Re-calculate layout and re-render

If using glerm, check if it handles resize events automatically.
