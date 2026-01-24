---
id: s-alp5
status: done
deps: [s-alp4]
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-alp1
tags: [ui, layout, agent-list, phase-1]
---
# Add create/destroy agent list pane functions

Add functions to create and destroy the agent list pane in layout.gleam.

```gleam
/// Create agent list pane at bottom with fixed height
pub fn create_agent_list_pane(layout: Layout, height: Int) -> Result(Layout, LayoutError)

/// Destroy agent list pane
pub fn destroy_agent_list_pane(layout: Layout) -> Result(Layout, LayoutError)

/// Toggle agent list pane visibility
pub fn toggle_agent_list_pane(layout: Layout, height: Int) -> Result(Layout, LayoutError)
```

The create function should:
1. Split at bottom of session
2. Run `scherzo agent-list` command in new pane
3. Resize to fixed height (5-7 rows, minimum 3)
4. Return updated Layout with agent_list_pane set
