---
id: s-f9f8
status: open
deps: []
links: []
created: 2026-01-20T04:53:33Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-8e87
tags: [tmux, phase-4]
---
# tmux session lifecycle management

Implement tmux session create, attach, and destroy operations in a new src/scherzo/ui/tmux.gleam module. Use shellout to call tmux commands:
- tmux new-session -d -s scherzo (create detached session)
- tmux attach-session -t scherzo (attach to existing)
- tmux kill-session -t scherzo (destroy session)
- tmux has-session -t scherzo (check if exists)

Include proper error handling for cases like session already exists or doesn't exist.

