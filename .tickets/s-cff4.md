---
id: s-cff4
status: open
deps: []
links: []
created: 2026-01-20T03:20:45Z
type: chore
priority: 3
assignee: Brian Romanko
tags: [error-handling, clarity]
---
# Fix generic Enoent error in store.gleam load_state

At store.gleam:197, load_state returns Enoent for any error condition, not just 'file not found'. This masks other file system errors like permission issues.

## Acceptance Criteria

Return actual error or more specific message
Distinguish between 'not found' and other errors
Improve debuggability

