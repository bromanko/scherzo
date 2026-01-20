---
id: s-b2c5
status: open
deps: []
links: []
created: 2026-01-20T02:14:55Z
type: bug
priority: 0
assignee: Brian Romanko
tags: [critical, state]
---
# Implement state persistence in store.gleam

The persist_state() and load_state() functions in src/scherzo/state/store.gleam are stubs that don't actually save/load JSON. State is never persisted to disk, tasks and agent state are lost on restart.

## Acceptance Criteria

- JSON encoding for tasks and agents using gleam/json
- Write to .scherzo/state/tasks.json and .scherzo/state/agents.json  
- Implement proper loading with JSON decoding
- Handle file not found gracefully (return empty state)

