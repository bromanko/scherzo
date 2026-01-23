---
id: s-alpg
status: open
deps: [s-alpb]
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 3
assignee: Brian Romanko
parent: s-alp1
tags: [ui, agent-list, phase-4]
---
# Add scrolling for many agents

When there are more agents than can fit in the pane (5-7 rows minus header), add scrolling.

Implementation:
- Track a "viewport offset" in state
- When selection moves past visible area, adjust offset to keep selection visible
- Optionally add scroll indicators (e.g., `▲` at top, `▼` at bottom when more items exist)
- Consider adding Page Up/Page Down keys for faster navigation

The selection should always be visible within the viewport.
