---
id: s-b8cf
status: closed
deps: []
links: []
created: 2026-01-20T02:14:56Z
type: bug
priority: 2
assignee: Brian Romanko
tags: [checkpoint, parser]
---
# Fix jj status parser vulnerability

In src/scherzo/agent/checkpoint.gleam:389-420, parse_jj_status() assumes perfectly formatted jj output. Malformed jj output (e.g., status with 2+ chars, tab-separated) will silently fail to parse.

## Acceptance Criteria

- Handle multiple spaces/tabs between status and path
- Validate status character is one of known values (A, M, D, R)
- Skip malformed lines with warning instead of failing silently

