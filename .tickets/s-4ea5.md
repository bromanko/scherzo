---
id: s-4ea5
status: open
deps: []
links: []
created: 2026-01-21T19:00:00Z
type: task
priority: 2
assignee:
parent: s-1900
tags: [cli, ux, phase-5]
---
# Group tasks by status in output

Modify `get_tasks()` in `src/scherzo/ui/commands.gleam` to group tasks by status instead of showing a flat list.

Output format:
```
=== In Progress (2) ===
s-4c84 Wire focus command to layout manager

=== Pending (8) ===
s-2443 Split large checkpoint.gleam module
s-b2f5 Replace custom int_to_string with stdlib
...

=== Completed (40) ===
s-368c Dynamic pane management
...
```

Implementation:
- Partition tasks into status groups using `list.partition` or `list.group`
- Define display order: in_progress > pending/ready > blocked > completed > failed
- Show count in section headers
- Add blank line between sections for readability

