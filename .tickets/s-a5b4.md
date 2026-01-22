---
id: s-a5b4
status: open
deps: [s-0571]
links: []
created: 2026-01-22T00:34:23Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-c0e5
tags: [phase-5, gates, types]
---
# Define gate types (Finding, Priority, Location, GateFeedback, GateResult)

Create gates/types.gleam with gate-related types:
- Finding: individual issue found by a gate
- Priority: P0Critical, P1Major, P2Minor, P3Suggestion
- Location: file/line reference for findings
- GateFeedback: structured feedback for agent
- GateResult: AllPassed or Failed with findings
- SingleGateResult: result of individual gate

## Acceptance Criteria

- gates/types.gleam exists with all types
- Types support JSON encoding for persistence
- Well-documented with examples
- Unit tests for encoding/decoding

