---
id: s-bc4d
status: closed
deps: [s-1472, s-af50]
links: []
created: 2026-01-20T02:02:07Z
type: task
priority: 2
assignee: Brian Romanko
tags: [phase3, sync]
---
# Implement status sync

Update ticket status when tasks complete or fail.

When orchestrator reports task result:
- Success -> call 'tk close <id>'
- Failure -> add note with error, keep open
- In Progress -> call 'tk start <id>'

Also update jj change description to reference ticket ID.

Add to orchestrator:
- on_task_start(task_source, task_id)
- on_task_complete(task_source, task_id, result)

