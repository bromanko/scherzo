---
id: s-b2f5
status: open
deps: []
links: []
created: 2026-01-20T03:16:30Z
type: chore
priority: 3
assignee: Brian Romanko
tags: [refactor, stdlib]
---
# Replace custom int_to_string with stdlib

At orchestrator.gleam:209-213, a custom int_to_string external function is defined when gleam/int.to_string exists in the standard library. Non-idiomatic and unnecessary.

## Acceptance Criteria

Remove custom @external int_to_string function
Use gleam/int.to_string instead
Verify all usages are updated

