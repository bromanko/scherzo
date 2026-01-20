---
id: s-aea8
status: open
deps: []
links: []
created: 2026-01-20T02:14:55Z
type: bug
priority: 1
assignee: Brian Romanko
tags: [event-bus]
---
# Fix Subject equality in event bus unsubscribe

In src/scherzo/event/bus.gleam:74-79, Subject comparison with \!= may not work reliably. Subjects are opaque Erlang processes - comparing them by value is undefined behavior. Unsubscribe may fail silently, leaving subscribers in the bus.

## Acceptance Criteria

- Use subscriber_id for lookup instead of Subject comparison
- Return the subscriber ID from subscribe() for later unsubscribe
- Verify unsubscribe actually removes subscribers

