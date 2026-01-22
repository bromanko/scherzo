---
id: s-2d0f
status: open
deps: []
links: []
tags: []
created: 2026-01-22T22:01:10Z
type: feature
priority: 2
assignee: Brian Romanko
---
# Combine agent prompts from template and repo-level CLAUDE.md/AGENTS.md files

Agent system prompts need to be a combination of:
1. Whatever's in the template .scherzo directory claude file for that agent
2. Repo-level CLAUDE.md or AGENTS.md files

**Issue:**
When we create workspaces and insert these files, we'll run into file conflicts because one will overwrite the other. The agent prompt needs to be a combination of both system prompts.

**Potential Solution:**
Use hooks to inject CLAUDE.md content instead of overwriting the file. This way we can combine both sources rather than having one clobber the other.

