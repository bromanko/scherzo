---
id: s-alph
status: open
deps: [s-alpb]
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 3
assignee: Brian Romanko
parent: s-alp1
tags: [ui, agent-list, phase-4]
---
# Add help screen

When user presses `?` in the agent list, show a help overlay with key bindings.

Help content:
```
Agent List Help
───────────────
j/↓     Move down
k/↑     Move up
Enter   Focus agent pane
z       Zoom agent pane
Esc     Return to REPL
q       Close agent list
r       Refresh
?       This help

Press any key to close
```

Implementation:
- Render help overlay on top of agent list
- Any keypress dismisses the help and returns to normal view
