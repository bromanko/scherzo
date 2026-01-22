---
id: s-ed21
status: open
deps: []
links: []
created: 2026-01-22T00:33:00Z
type: epic
priority: 2
assignee: Brian Romanko
parent: s-6347
tags: [phase-5, refactor]
---
# 5a: Orchestrator Refactor

Split orchestrator.gleam into coordinator and supervisor modules for cleaner separation of concerns before adding gate integration.

## Acceptance Criteria

orchestrator.gleam split into orchestrator/supervisor.gleam and orchestrator/coordinator.gleam with no behavior changes

