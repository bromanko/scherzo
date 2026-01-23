---
id: s-alpc
status: open
deps: [s-alp8, s-alpa]
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-alp1
tags: [ui, agent-list, phase-2]
---
# Implement agent state polling

Add polling loop to the agent list that calls `scherzo agents --json` every 1 second.

Implementation:
1. Spawn a timer/interval that triggers every 1000ms
2. On each tick, shell out to `scherzo agents --json`
3. Parse the JSON response
4. Update the agent list state
5. Re-render the TUI

Error handling:
- If the CLI call fails, display error inline: `Error: Failed to fetch agents - retrying...`
- Continue retrying on subsequent poll cycles
- Don't crash the TUI on transient errors

The polling should not block keyboard input - use async/concurrent approach if needed.
