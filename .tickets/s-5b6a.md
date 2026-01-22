---
id: s-5b6a
status: open
deps: [s-b1aa]
links: []
created: 2026-01-22T00:33:45Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-6090
tags: [phase-5, config, formulas]
---
# Implement formula loader for default gate configurations

Create config/loader.gleam to load TOML formula files from config/formulas/:
- code-review.toml (comprehensive 7-dimension review)
- security-audit.toml (security-focused)
- quick-review.toml (fast feedback)

Merge formula defaults with user overrides from .scherzo/config.toml.

## Acceptance Criteria

- config/loader.gleam exists
- Loads formula files at runtime
- Merges user config with formula defaults (user wins on conflict)
- Falls back gracefully if formula file missing
- Unit tests cover merge logic

