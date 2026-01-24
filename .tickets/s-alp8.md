---
id: s-alp8
status: done
deps: []
links: []
created: 2026-01-23T00:00:00Z
type: task
priority: 2
assignee: Brian Romanko
parent: s-alp1
tags: [cli, agent-list, phase-2]
---
# Add --json flag to agents command

Add a `--json` flag to the existing `scherzo agents` CLI command to output structured JSON.

The agent list pane will poll this command every 1 second to get current agent state.

JSON output format:
```json
{
  "agents": [
    {
      "id": "agent-1234567890",
      "name": "fine-elk-32",
      "status": "running",
      "task_id": "task-1769208524758494003",
      "provider": "claude"
    }
  ]
}
```

The `name` field should use the existing `names.from_agent_id()` function.
