---
id: s-2eaf
status: open
deps: []
links: []
created: 2026-01-20T03:11:51Z
type: bug
priority: 0
assignee: Brian Romanko
tags: [critical, security, workspace]
---
# Fix path traversal vulnerability in workspace sanitize_id

At workspace.gleam:61, sanitize_id() only replaces /, space, and colon characters. It doesn't handle '..' sequences, allowing directory traversal attacks. A task ID like 'task-../../../tmp' could escape the workspace base directory.

## Acceptance Criteria

Reject absolute paths entirely
Reject '..' and '.' path components
Use whitelist of safe characters (alphanumeric, dash, underscore)
Validate against canonical path to ensure no traversal
Add tests with malicious task IDs

