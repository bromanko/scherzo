---
id: s-alp6
status: open
deps: [s-alp2, s-alp4]
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-alp1
tags: [ui, layout, agent-list, phase-1]
---
# Exclude agent list pane from tiled layout

Modify layout tiling to preserve the agent list pane's fixed height.

When `apply_tiled_layout` runs:
1. Apply tiled layout to the session (affects all panes including agent list)
2. If agent_list_pane is Some, re-apply fixed height using resize_pane_height

This ensures agent panes tile nicely while the agent list stays at a fixed height at the bottom.

Alternative approach: Only apply tiled layout to agent panes by targeting them specifically, but this is more complex.
