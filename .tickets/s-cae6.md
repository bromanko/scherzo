---
id: s-cae6
status: open
deps: []
links: []
created: 2026-01-20T03:20:39Z
type: chore
priority: 3
assignee: Brian Romanko
tags: [style, consistency]
---
# Standardize error message formatting across codebase

Error messages use inconsistent styles: 'Failed to create checkpoints directory: ' vs 'Failed to read checkpoint: ' vs 'Failed to read checkpoint JSON'. Some include error details, some don't.

## Acceptance Criteria

Establish consistent error message format pattern
All errors should include: action, context, and error details where available
Document the pattern in a comment or CONTRIBUTING guide

