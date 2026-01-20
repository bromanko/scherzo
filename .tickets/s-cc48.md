---
id: s-cc48
status: closed
deps: []
links: []
created: 2026-01-20T03:14:43Z
type: bug
priority: 1
assignee: Brian Romanko
tags: [event-bus, error-handling]
---
# Fix unsafe assertion in event bus subscribe

At event/bus.gleam:45, 'let assert Ok(id) = process.receive(reply_subject, 5000)' will panic the entire process if the receive times out. No graceful error handling.

## Acceptance Criteria

Use case expression instead of let assert
Handle timeout gracefully (log error, return Result)
Consider if subscription failure should be fatal or recoverable

