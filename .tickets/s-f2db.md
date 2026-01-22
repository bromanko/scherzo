---
id: s-f2db
status: open
deps: [s-ea4f, s-3172, s-69a8, s-573b]
links: []
created: 2026-01-22T00:36:36Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-4b13
tags: [phase-5, ui, status]
---
# Add gate evaluation status to control pane

Extend control pane to show gate evaluation status:
- Show when task is in Evaluating phase
- Display which gate is currently running
- Show NeedsFixes state with iteration count
- Show Stuck state with last feedback summary
- Add gate-related commands (approve, reject for human gates)

Update ui/commands.gleam and ui/repl.gleam.

## Acceptance Criteria

- Status command shows gate phase info
- Evaluating tasks show current gate
- NeedsFixes shows iteration count
- Stuck tasks clearly visible
- Approve/reject commands work for human gates
- Tests cover new UI states

