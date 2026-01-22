---
id: s-5bf4
status: closed
deps: [s-5b6a]
links: []
created: 2026-01-22T00:33:52Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-6090
tags: [phase-5, config, validation]
---
# Implement config validation

Add validation to config loader to ensure:
- Gates are valid (known types)
- Prompts are non-empty
- No circular dependencies between gates
- Retry config has valid values (max_iterations > 0)
- Review dimensions have required fields

## Acceptance Criteria

- Validation runs on config load
- Clear error messages for each validation failure
- Invalid configs rejected before gates run
- Unit tests cover validation rules

