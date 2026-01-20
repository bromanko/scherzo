---
id: s-ff20
status: closed
deps: []
links: []
created: 2026-01-20T03:14:59Z
type: bug
priority: 1
assignee: Brian Romanko
tags: [security, testing, claude-settings]
---
# Add validation tests for task ID sanitization in claude_settings

At claude_settings.gleam:66,93,119, task IDs are embedded in shell commands. While sanitize function exists, there are no tests verifying malicious task IDs are properly rejected. Need explicit validation tests.

## Acceptance Criteria

Add test cases with shell metacharacters in task IDs
Verify sanitization rejects dangerous characters
Log rejected task IDs for audit purposes
Consider additional encoding layer

