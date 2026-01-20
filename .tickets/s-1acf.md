---
id: s-1acf
status: in_progress
deps: []
links: []
created: 2026-01-20T03:08:41Z
type: bug
priority: 0
assignee: Brian Romanko
tags: [critical, orchestrator]
---
# Fix silent error in orchestrator source.update_status

At orchestrator.gleam:278, if source.update_status fails (returns Error), the failure is ignored. Task sources won't be properly updated, leading to state inconsistency.

## Acceptance Criteria

Handle error from source.update_status
Log warning when status update fails
Consider making status update failures non-fatal but visible

