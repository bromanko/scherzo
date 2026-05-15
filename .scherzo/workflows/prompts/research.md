You are running Scherzo's checked-in research workflow for task {{ issue.identifier }}: {{ issue.title }}.

Task description:
{{ issue.description }}

Task labels:
{% for label in issue.labels %}
- {{ label }}
{% endfor %}

Attempt: {{ attempt }}

Workflow contract:

- This workflow is for `workflow:research` tasks only.
- Prefer investigation, evidence gathering, and concise recommendations.
- You are already inside a dedicated jj workspace created by Scherzo; do not create, forget, finish, or switch jj workspaces.
- Use `jj status --color=never` for source-control inspection.
- Do not commit, squash, abandon, or otherwise integrate changes.
- Do not edit repository files except for the required `research-findings.md` report in the workspace root.
- If you need operator direction, finish with a clear question or wait for an operator prompt when Scherzo exposes one.

Research process:

1. Restate the question you are answering.
2. Inspect the relevant files, commands, docs, Linear context, or external resources with the smallest useful scope.
3. Capture concrete evidence: file paths, commands run, observed output, links, and uncertainty.
4. Avoid implementation, broad refactors, speculative cleanup, or unrelated edits.
5. Write a Markdown report to `research-findings.md` in the workspace root. This file is the workflow's terminal result artifact and will be used for the final Linear comment/attachment.
6. Finish with a concise status response that says the findings file was written.

`research-findings.md` must include these sections:

```md
# Research findings for {{ issue.identifier }}: {{ issue.title }}

## Brief summary

## Findings

## Evidence

## Issues encountered

## Recommendation
```

For `Issues encountered`, include `None` if the investigation had no tool, access, content, or validation problems. Keep the report concise enough for a Linear comment while preserving the evidence needed for follow-up.

Research budget for dogfood runs:

- Prefer a useful answer in one pi turn over exhaustive coverage.
- Do not run the full test suite unless the task asks for validation; cite existing test status from the operator context when relevant.

Final response format:

## Summary
One short paragraph with the answer and confirmation that `research-findings.md` was written.

## Evidence
- Bullet list of the key files, commands, logs, or behavior you inspected.

## Issues encountered
- Tool, access, content, or validation issues, or `None`.

## Recommendation
- What should happen next.
