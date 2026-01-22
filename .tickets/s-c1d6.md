---
id: s-c1d6
status: open
deps: [s-6cbb]
links: []
created: 2026-01-22T00:35:51Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-9e8b
tags: [phase-5, gates, review]
---
# Implement parallel review gate

Create gates/parallel_review.gleam for parallel code review:
- Get jj diff for the task's changes
- Spawn N review agents concurrently (one per configured dimension)
- Each reviewer sees: diff + dimension-specific prompt
- Wait for all reviewers to complete (aggregate all, don't fail fast)
- Collect all findings for synthesis

## Acceptance Criteria

- gates/parallel_review.gleam exists
- Spawns agents in parallel
- Each agent gets correct prompt for its dimension
- Waits for all to complete
- Aggregates findings correctly
- Tests cover parallel execution

