---
id: s-ea72
status: closed
deps: []
links: []
created: 2026-01-20T02:14:55Z
type: bug
priority: 1
assignee: Brian Romanko
tags: [handoff]
---
# Fix int_to_string() bug in handoff.gleam

Custom int_to_string() function at src/scherzo/agent/handoff.gleam:145-162 produces reversed digits and handles negatives incorrectly. Should use int.to_string() from Gleam's standard library instead.

## Acceptance Criteria

- Replace custom int_to_string() with int.to_string() from stdlib
- Remove the custom implementation entirely
- Verify continuation prompts show correct numbers

