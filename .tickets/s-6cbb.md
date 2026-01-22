---
id: s-6cbb
status: open
deps: [s-45f2]
links: []
created: 2026-01-22T00:35:21Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-6c75
tags: [phase-5, gates, coordinator]
---
# Wire gate completion signals into coordinator

Integrate gate execution into coordinator (orchestrator/coordinator.gleam):
- Listen for TaskCompletionSignaled events
- Trigger GateExecutor when agent signals done
- Handle gate results (pass → merge queue, fail → retry/stuck)
- Manage task phase transitions
- Connect to retry controller for retry decisions

## Acceptance Criteria

- Coordinator triggers gates on completion signal
- Pass/fail handled correctly
- Task phases transition properly
- Retry logic invoked when needed
- Integration tests cover full flow

