---
id: s-alp2
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
# Add resize_pane_height to tmux.gleam

Add a function to resize a tmux pane to a specific height.

```gleam
/// Resize a pane to a specific height
pub fn resize_pane_height(pane_id: PaneId, height: Int) -> Result(Nil, TmuxError)
```

Implementation: Call `tmux resize-pane -t <pane_id> -y <height>`

This is needed for the agent list pane to maintain fixed height after tiled layout is applied.
