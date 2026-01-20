---
id: s-8a9b
status: closed
deps: [s-8abf]
links: []
created: 2026-01-19T23:31:12Z
type: task
priority: 2
assignee: Brian Romanko
tags: [phase3, actor]
---
# Implement TaskQueue actor

Create src/scherzo/task/queue.gleam as an OTP actor managing the task queue.

Features:
- Priority queue (priority 0 = highest)
- Respects dependencies (only enqueue tasks from get_ready_tasks)
- Dequeue returns highest priority ready task
- Re-check dependencies when task completes

Messages:
- Enqueue(task) - add task to queue
- Dequeue(reply_to) - get next ready task
- Complete(task_id) - mark complete, re-evaluate queue
- Refresh - reload from TaskSource

State:
- pending: List(Task) sorted by priority
- in_progress: Dict(Id, Task)

