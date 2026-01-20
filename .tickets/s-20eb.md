---
id: s-20eb
status: open
deps: []
links: []
created: 2026-01-20T04:46:32Z
type: chore
priority: 3
assignee: Brian Romanko
tags: [refactor, code-quality]
---
# Refactor workspace.create to use result.try instead of nested case

The create function in workspace.gleam:65-105 has deeply nested case expressions for sequential fallible operations. Refactor to use 'use' expressions with result.try for cleaner, more readable chaining of Result operations.

