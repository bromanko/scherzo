---
id: s-5fd0
status: closed
deps: []
links: []
created: 2026-01-20T03:15:54Z
type: bug
priority: 2
assignee: Brian Romanko
tags: [parser, clarity]
---
# Make priority parsing fallback explicit in parser.gleam

At parser.gleam:107, when priority cannot be parsed, it keeps the accumulator value which may be None. The implicit fallback to priority 2 at line 51 isn't obvious from the parsing code.

## Acceptance Criteria

Make default priority explicit in parsing code
Add comment explaining the fallback behavior
Consider validating priority is within bounds (0-4)

