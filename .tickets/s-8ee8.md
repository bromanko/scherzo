---
id: s-8ee8
status: open
deps: []
links: []
created: 2026-01-20T04:38:29Z
type: chore
priority: 3
assignee: Brian Romanko
tags: [refactor, warnings]
---
# Remove unused _workspace_dir parameter from create_checkpoint

The parameter _workspace_dir in create_checkpoint (src/scherzo.gleam:325) is unused and prefixed with underscore. Either use the parameter or remove it from the function signature if not needed.

