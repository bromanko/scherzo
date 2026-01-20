---
id: s-e77e
status: open
deps: []
links: []
created: 2026-01-20T03:16:26Z
type: chore
priority: 2
assignee: Brian Romanko
tags: [refactor, duplication]
---
# Extract duplicated JSON hook generation in claude_settings

claude_settings.gleam has three nearly identical functions (session_start_hooks, stop_hooks, pre_compact_hooks) at lines 54-76, 79-104, 107-129. Should be refactored to use a common pattern.

## Acceptance Criteria

Extract common hook generation pattern
Create parameterized helper function
Reduce duplication while maintaining clarity
Add tests for hook generation

