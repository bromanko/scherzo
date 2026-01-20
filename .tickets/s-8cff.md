---
id: s-8cff
status: open
deps: []
links: []
created: 2026-01-20T04:45:12Z
type: chore
priority: 3
assignee: Brian Romanko
tags: [refactor]
---
# Add helper for json.array with identity function

In claude_settings.gleam:56-75, 81-103, 109-128, the pattern json.array([...], fn(x) { x }) is repeated multiple times. Create a helper function to reduce verbosity: fn json_array(items: List(json.Json)) -> json.Json { json.array(items, fn(x) { x }) }

