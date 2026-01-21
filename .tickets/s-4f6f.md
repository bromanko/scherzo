---
id: s-4f6f
status: closed
deps: [s-4ea5, s-71a2]
links: []
created: 2026-01-21T19:00:00Z
type: task
priority: 3
assignee:
parent: s-1900
tags: [cli, ux, phase-5]
---
# Add hierarchy tree view for tasks

Add `--tree` flag to show tasks organized by parent/child relationships.

Example output:
```
=== Task Hierarchy ===
s-8e87 Phase 4: tmux UI
  s-f9f8 tmux session lifecycle management
  s-368c Dynamic pane management
  s-074f scherzo attach CLI command
  s-5137 scherzo console CLI command

s-1900 Tasks command UX improvements
  s-4ea5 Group tasks by status in output
  s-182a Add filtering flags to tasks command
  ...
```

Prerequisites:
- s-71a2 must be completed first (adds `parent` field to parser and Task type)

Implementation:
- Build tree structure from flat task list using `parent` field
- Add `--tree` flag to CLI
- Add `tree` argument support to REPL `tasks` command
- Implement tree rendering with indentation (2 spaces per level)
- Show orphan tasks (no parent) as top-level items
- Combine with status filtering (e.g., `--tree --pending`)
- Consider showing epic tasks differently (if s-63b8 is done)

