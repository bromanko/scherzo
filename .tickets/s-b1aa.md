---
id: s-b1aa
status: open
deps: [s-5020]
links: []
created: 2026-01-22T00:33:37Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-6090
tags: [phase-5, config, parser]
---
# Implement config parser for .scherzo/config.toml

Create config/parser.gleam to parse .scherzo/config.toml using the tom library. Handle:
- Gate list parsing
- Retry config parsing
- Review dimension parsing
- Error handling for malformed TOML

## Acceptance Criteria

- config/parser.gleam exists
- Parses valid .scherzo/config.toml files
- Returns helpful errors for invalid configs
- Unit tests cover parsing edge cases

