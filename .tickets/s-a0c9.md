---
id: s-a0c9
status: open
deps: []
links: []
created: 2026-01-20T04:39:24Z
type: chore
priority: 3
assignee: Brian Romanko
tags: [refactor]
---
# Simplify is_safe_char function in workspace.gleam

The is_safe_char function (src/scherzo/agent/workspace.gleam:276-291) uses an exhaustive pattern match for every safe character. Consider using a string membership check for cleaner code: string.contains("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_", c)

