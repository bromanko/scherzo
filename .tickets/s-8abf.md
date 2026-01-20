---
id: s-8abf
status: closed
deps: []
links: []
created: 2026-01-19T23:30:04Z
type: task
priority: 2
assignee: Brian Romanko
tags: [phase3, foundation]
---
# Define TaskSource interface

Create src/scherzo/task/source.gleam with the TaskSource type that abstracts task backends.

The interface should include:
- name: String
- fetch_tasks: fn() -> Result(List(Task), String)
- update_status: fn(Id, TaskStatus) -> Result(Nil, String)  
- get_ready_tasks: fn() -> Result(List(Task), String)

This enables pluggable task sources (Ticket now, Beans later).

