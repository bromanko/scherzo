---
id: s-8eeb
status: open
deps: []
links: []
created: 2026-01-20T03:20:43Z
type: chore
priority: 3
assignee: Brian Romanko
tags: [docs, types]
---
# Add documentation for ParsedOutput type in driver.gleam

ParsedOutput type at driver.gleam:19-29 has no documentation. Each variant (Output, FinalOutput, ContextExhausted, Error) should be documented explaining when it's returned.

## Acceptance Criteria

Add /// doc comments to ParsedOutput type
Document each variant's meaning and when it's used
Add usage examples if helpful

