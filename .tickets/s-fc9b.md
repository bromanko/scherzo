---
id: s-fc9b
status: open
deps: []
links: []
created: 2026-01-20T03:16:24Z
type: chore
priority: 2
assignee: Brian Romanko
tags: [refactor, duplication]
---
# Extract duplicated jj change description building

process.gleam:182-216 and orchestrator.gleam:184-198 have nearly identical logic for building jj change descriptions from agent results. Should be extracted to shared utility.

## Acceptance Criteria

Create shared utility module (e.g., vcs/jj_descriptions.gleam)
Extract common change description building logic
Update both callers to use shared function
Add tests for the shared function

