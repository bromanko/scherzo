---
id: s-ce48
status: closed
deps: []
links: []
created: 2026-01-20T04:53:59Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-8e87
tags: [repl, phase-4]
---
# Control REPL skeleton

Implement control REPL infrastructure in src/scherzo/ui/repl.gleam:
- Read input from stdin in a loop
- Parse commands (split on whitespace, extract command + args)
- Command registry mapping command names to handlers
- Display 'scherzo> ' prompt
- Handle empty input gracefully

The REPL runs as an actor that receives parsed commands and dispatches to handlers.

