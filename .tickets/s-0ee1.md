---
id: s-0ee1
status: open
deps: [s-af50, s-8a9b, s-bc4d]
links: []
created: 2026-01-20T02:02:17Z
type: task
priority: 2
assignee: Brian Romanko
tags: [phase3, integration]
---
# Integrate TaskSource with orchestrator

Wire TaskSource and TaskQueue into the orchestrator.

Changes to orchestrator:
- Accept TaskSource in config
- Add 'run_from_source' mode that:
  1. Calls get_ready_tasks() to populate queue
  2. Dequeues and runs tasks until queue empty
  3. Updates status after each task
  4. Refreshes queue after completions

CLI changes:
- 'scherzo run --from-tickets' to process .tickets/
- 'scherzo run --max-tasks N' to limit batch size

