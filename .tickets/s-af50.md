---
id: s-af50
status: closed
deps: [s-8abf, s-1472, s-4430]
links: []
created: 2026-01-19T23:30:52Z
type: task
priority: 2
assignee: Brian Romanko
tags: [phase3, adapter]
---
# Implement TicketSource adapter

Create src/scherzo/task/sources/ticket.gleam implementing the TaskSource interface.

Combines tk CLI wrapper + ticket parsing to provide:
- fetch_tasks() - use 'tk query --json' to get all tickets, convert to Tasks
- update_status() - call 'tk start/close' based on new status
- get_ready_tasks() - use 'tk ready' to get dependency-resolved tickets

This is the primary TaskSource for Scherzo.

