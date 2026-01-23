---
id: s-alp3
status: open
deps: []
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-alp1
tags: [ui, tmux, agent-list, phase-1]
---
# Add bind_key function to tmux.gleam

Add a function to bind a key at the tmux session level.

```gleam
/// Bind a key at the session level
pub fn bind_key(session: String, key: String, command: String) -> Result(Nil, TmuxError)
```

Implementation: Call `tmux bind-key -t <session> <key> <command>`

This is needed for the F1 toggle hotkey that works from any pane.
