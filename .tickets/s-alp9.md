---
id: s-alp9
status: done
deps: []
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-alp1
tags: [cli, agent-list, phase-2]
---
# Add scherzo agent-list subcommand

Add a new `scherzo agent-list` subcommand that runs the agent list TUI.

This command:
1. Runs in the foreground (blocking)
2. Takes over the terminal for TUI rendering
3. Exits when user presses `q` or the pane is killed

The subcommand should be registered in scherzo.gleam alongside existing commands.

This is the entry point for the agent list pane - it gets invoked when the pane is created.
