---
id: s-ec9d
status: open
deps: []
links: []
created: 2026-01-20T03:15:20Z
type: task
priority: 1
assignee: Brian Romanko
tags: [reliability, timeout]
---
# Add timeouts to all shellout.command calls

Multiple locations use shellout.command without timeout: orchestrator.gleam:164-175, process.gleam:156-174, jj.gleam:99, tk.gleam:26,34,58-75. If any external command hangs, the entire orchestrator blocks indefinitely with no recovery.

## Acceptance Criteria

Add timeout_ms to CommandConfig type
Implement timeout wrapper for shell commands
Configure different timeouts per command type (Claude: 30+ min, jj: 60s, tk: 30s)
Add configurable timeout in OrchestratorConfig
Return Interrupted result on timeout

