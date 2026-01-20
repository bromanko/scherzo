---
id: s-0a1e
status: open
deps: []
links: []
created: 2026-01-20T03:15:56Z
type: bug
priority: 2
assignee: Brian Romanko
tags: [parser, security, validation]
---
# Add input validation to ticket parser

parser.gleam lacks validation of parsed values: no max length checks for title/description, no ID format validation, no bounds checking for priority, no tag format validation. Could cause memory issues or unexpected behavior.

## Acceptance Criteria

Add length limits: TITLE_MAX=256, DESCRIPTION_MAX=10000, ID_MAX=128
Validate priority is 0-4
Validate IDs match whitelist pattern
Reject unreasonably long lists (max 50 deps/tags)
Log warnings for malformed tickets

