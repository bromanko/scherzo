---
id: s-e23d
status: closed
deps: []
links: []
created: 2026-01-20T02:14:56Z
type: bug
priority: 1
assignee: Brian Romanko
tags: [security]
---
# Fix shell injection vulnerability in task IDs

In src/scherzo/agent/claude_settings.gleam:36, task ID is concatenated directly into shell command without validation or escaping. Malicious task IDs could potentially inject commands.

## Acceptance Criteria

- Validate task IDs to only contain safe characters (alphanumeric, dash, underscore)
- Alternatively, shell-escape the task ID before use
- Add validation in task creation

