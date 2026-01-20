---
id: s-ff47
status: open
deps: []
links: []
created: 2026-01-20T03:15:02Z
type: bug
priority: 1
assignee: Brian Romanko
tags: [security, jj, orchestrator]
---
# Sanitize task titles in jj change descriptions

At orchestrator.gleam:126,186, task titles are directly concatenated into jj change descriptions without escaping. Task titles come from user input and could contain newlines or special characters causing unexpected jj behavior.

## Acceptance Criteria

Validate/sanitize task titles for shell-safe characters
Escape or remove newlines from titles
Add length limits for task titles
Test with malicious title content

