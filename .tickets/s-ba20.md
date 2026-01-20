---
id: s-ba20
status: open
deps: []
links: []
created: 2026-01-20T02:29:24Z
type: task
priority: 3
assignee: Brian Romanko
tags: [testing, agent, settings]
---
# Add unit tests for agent/claude_settings.gleam

The claude_settings module generates Claude Code settings JSON for autonomous operation but has no test coverage. Functions to test: generate_autonomous_settings(), hook generation for SessionStart/Stop/PreCompact events.

## Acceptance Criteria

Test generate_autonomous_settings() returns valid JSON
Test generated JSON contains required hook configurations
Test SessionStart hook has correct command structure
Test Stop hook has correct command structure
Test PreCompact hook has correct command structure
Test workspace_path is correctly embedded in hook commands

