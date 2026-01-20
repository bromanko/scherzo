---
id: s-5bfd
status: closed
deps: []
links: []
created: 2026-01-20T02:30:21Z
type: task
priority: 4
assignee: Brian Romanko
tags: [testing, core, event]
---
# Add unit tests for core/event.gleam

The event module defines event types and provides wrapper functions for the event system but has no test coverage. Functions to test: wrap() which wraps domain events with timestamp metadata.

## Acceptance Criteria

Test wrap() adds timestamp to event
Test wrap() preserves original event data
Test timestamp is valid and recent
Test all event type constructors if any helper functions exist

