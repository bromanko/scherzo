---
id: s-eb9f
status: open
deps: []
links: []
created: 2026-01-20T04:44:29Z
type: chore
priority: 3
assignee: Brian Romanko
tags: [refactor, code-quality]
---
# Extract timeout magic numbers to named constants

Several hardcoded timeout values (60_000, 5000, 30_000) are scattered throughout the codebase in process.gleam:68, queue.gleam:64,82,87. Extract these to named constants in core/types.gleam for clarity and maintainability (e.g., call_timeout_ms, start_task_timeout_ms, refresh_timeout_ms).

