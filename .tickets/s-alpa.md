---
id: s-alpa
status: done
deps: [s-alp9]
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-alp1
tags: [ui, tui, agent-list, phase-2]
---
# Create agent_list.gleam module with glerm TUI

Create `src/scherzo/ui/agent_list.gleam` module that implements the agent list TUI using glerm.

Rendering:
```
┌─ Agents (3) ────────────────────────────────────────────────┐
│   fine-elk-32    ● running   task-1769208524758494003       │
│ ▶ cool-fox-17    ○ idle      task-1769210740397194018       │
│   swift-owl-88   ◐ waiting   task-1769211234567890123       │
└─────────────────────────────────────────────────────────────┘
```

Features:
- Header with agent count
- Status indicators: `●` running, `○` idle, `◐` waiting, `✓` done, `✗` failed
- Task ID (truncated if needed)
- Selected row highlighted with `▶` marker or inverted colors

State:
- List of agents (from polling)
- Currently selected index
- Error message (if last poll failed)
