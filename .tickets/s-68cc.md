---
id: s-68cc
status: wontfix
deps: []
links: []
created: 2026-01-20T03:15:51Z
type: bug
priority: 2
assignee: Brian Romanko
tags: [process, compilation]
---
# Add missing types module import in process.gleam

At process.gleam:119, types.AgentConfig is used without importing the types module. Should be caught at compile-time but indicates incomplete import management.

## Acceptance Criteria

Add import scherzo/core/types
Verify all type references are properly imported
Run gleam check to verify

