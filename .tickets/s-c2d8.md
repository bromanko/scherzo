---
id: s-c2d8
status: closed
deps: []
links: []
created: 2026-01-20T03:45:29Z
type: task
priority: 2
assignee: Brian Romanko
tags: [security]
---
# Review note content handling in tk.gleam add_note

The add_note() function in tk.gleam:74 passes user-provided note content directly to the tk CLI without validation.

## Location
src/scherzo/task/sources/tk.gleam:74

## Current Code
```gleam
pub fn add_note(id: String, note: String) -> Result(Nil, String) {
  case run_tk(["add-note", id, note]) {
```

## Mitigating Factor
Uses shellout.command() with args as a List, not shell interpolation, so shell injection is not possible.

## Recommendation
Consider:
1. Limiting note character set or length
2. Escaping special characters that might affect tk's behavior
3. Documenting expected input format

