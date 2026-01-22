---
id: s-5ddf
status: open
deps: [s-4a82]
links: []
created: 2026-01-22T00:34:59Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-6c75
tags: [phase-5, gates, retry]
---
# Implement retry controller

Create gates/retry.gleam with retry logic:
- Track iteration count per task
- Decide same-agent vs fresh-agent based on config
- Record iteration history for debugging
- Implement should_retry() and should_use_fresh_agent() functions

Uses RetryConfig from config/types.gleam.

## Acceptance Criteria

- gates/retry.gleam exists
- Tracks iterations correctly
- Same-agent by default, fresh after N failures per config
- Iteration history persisted
- Unit tests cover retry decisions

