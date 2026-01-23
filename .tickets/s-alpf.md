---
id: s-alpf
status: open
deps: [s-alpb]
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-alp1
tags: [ui, agent-list, phase-3]
---
# Wire Esc key to return to control pane

When user presses Esc in the agent list, return focus to the control pane.

Implementation:
1. Shell out to `scherzo focus control` (or similar command to focus control pane)
2. Handle errors

This provides a quick way to get back to the REPL from the agent list.

May need to add support for `scherzo focus control` if it doesn't exist yet.
