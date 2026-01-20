---
id: s-8acb
status: closed
deps: [s-ce48]
links: []
created: 2026-01-20T04:54:04Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-8e87
tags: [repl, phase-4]
---
# Control commands: status, tasks, agents

Implement informational control pane commands:
- status: Show overall status (active agents, pending/completed/failed counts)
- tasks: List all tasks with their status
- agents: Show agent status (id, task, state)

Commands query orchestrator/state store and format output for display in control pane.

