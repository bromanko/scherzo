---
id: s-0b7d
status: open
deps: []
links: []
created: 2026-01-20T04:45:31Z
type: chore
priority: 3
assignee: Brian Romanko
tags: [cli, error-handling]
---
# Use stderr and exit codes for CLI error handling

In scherzo.gleam:143-146, 158-168, error cases print to stdout without exit codes, making it hard to distinguish success from failure programmatically. Use stderr for error messages and add appropriate exit codes for failures.

