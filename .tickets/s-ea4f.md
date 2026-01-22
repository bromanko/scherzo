---
id: s-ea4f
status: open
deps: [s-6cbb]
links: []
created: 2026-01-22T00:35:43Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-9e8b
tags: [phase-5, gates, command]
---
# Implement command gate

Create gates/command.gleam for running shell commands as gates:
- Run configured command (tests, lint, typecheck)
- Capture stdout/stderr
- Parse exit code for pass/fail
- Extract findings from output if possible (e.g., test failures, lint errors)
- Configurable timeout

## Acceptance Criteria

- gates/command.gleam exists
- Runs shell commands via shellout
- Returns pass/fail based on exit code
- Captures output for feedback
- Handles timeouts gracefully
- Unit tests cover success/failure/timeout

