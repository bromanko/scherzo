---
id: s-alp4
status: open
deps: [s-alp2, s-alp3]
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-alp1
tags: [ui, layout, agent-list, phase-1]
---
# Add agent_list_pane to Layout type

Extend the Layout type to track the agent list pane separately from agent panes.

```gleam
pub type Layout {
  Layout(
    session: String,
    control_pane: PaneConfig,
    agent_panes: Dict(String, PaneConfig),
    agent_list_pane: Option(PaneConfig),  // NEW
  )
}
```

The agent list pane is optional because it can be toggled on/off with F1.

Update all existing Layout construction sites to include `agent_list_pane: None`.
