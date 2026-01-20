---
id: s-deb3
status: closed
deps: [s-2449]
links: []
created: 2026-01-20T04:54:40Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-8e87
tags: [testing, phase-4]
---
# Unit tests for control REPL

Write unit tests for src/scherzo/ui/repl.gleam in test/scherzo/ui/repl_test.gleam:
- Test command parsing (various input formats)
- Test command registry lookup
- Test argument extraction
- Test error handling for invalid commands
- Test empty input handling

Focus on parsing logic; integration tests cover actor behavior.

