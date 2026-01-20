---
id: s-d42f
status: closed
deps: [s-368c]
links: []
created: 2026-01-20T04:54:35Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-8e87
tags: [testing, phase-4]
---
# Unit tests for tmux module

Write unit tests for src/scherzo/ui/tmux.gleam in test/scherzo/ui/tmux_test.gleam:
- Test command building (verify correct tmux command strings)
- Test session name sanitization
- Test pane ID parsing
- Mock shellout for isolated testing

Focus on pure functions; integration tests cover actual tmux interaction.

